// Copyright 2019 Google LLC.
// SPDX-License-Identifier: Apache-2.0
//
// sync-gcsr implements a small utility that periodically mirrors a
// remote Google Cloud Source Repository to a local file path.
//
// This utility is also responsible for triggering depot builds on
// builds.sr.ht if a change is detected on the master branch.
package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"time"
	"bytes"

	git "gopkg.in/src-d/go-git.v4"
	"gopkg.in/src-d/go-git.v4/plumbing"
	githttp "gopkg.in/src-d/go-git.v4/plumbing/transport/http"
)

// Path to the build manifest, added by Nix at compile time.
var BuildManifest string

// Represents a builds.sr.ht build object as described on
// https://man.sr.ht/builds.sr.ht/api.md
type Build struct {
	Manifest string   `json:"manifest"`
	Note     string   `json:"note"`
	Tags     []string `json:"tags"`
}

func EnvOr(key, def string) string {
	v := os.Getenv(key)
	if v == "" {
		return def
	}

	return v
}

// Trigger a build of master on builds.sr.ht
func triggerBuild(commit string) {
	manifest, err := ioutil.ReadFile(BuildManifest)
	if err != nil {
		log.Fatalln("[ERROR] failed to read sr.ht build manifest:", err)
	}

	build := Build{
		Manifest: string(manifest),
		Note:     fmt.Sprintf("Build of 'master' at '%s'", commit),
		Tags: []string{
			"tazjins-depot", "master",
		},
	}

	body, _ := json.Marshal(build)
	reader := ioutil.NopCloser(bytes.NewReader(body))

	req, err := http.NewRequest("POST", "https://builds.sr.ht/api/jobs", reader)
	if err != nil {
		log.Fatalln("[ERROR] failed to create an HTTP request:", err)
	}

	req.Header.Add("Authorization", fmt.Sprintf("token %s", os.Getenv("SRHT_TOKEN")))
	req.Header.Add("Content-Type", "application/json")

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		// This might indicate a temporary error on the SourceHut side, do
		// not fail the whole program.
		log.Println("failed to send builds.sr.ht request:", err)
		return
	}
	defer resp.Body.Close()

	if resp.StatusCode != 200 {
		respBody, err := ioutil.ReadAll(resp.Body)
		log.Printf("received non-success response from builds.sr.ht: %s (%v)[%s]", respBody, resp.Status, err)
	} else {
		log.Println("triggered builds.sr.ht job for commit", commit)
	}
}

// ensure that all remote branches exist locally & are up to date.
func updateBranches(auth *githttp.BasicAuth, repo *git.Repository) error {
	origin, err := repo.Remote("origin")
	if err != nil {
		return err
	}

	refs, err := origin.List(&git.ListOptions{
		Auth: auth,
	})
	if err != nil {
		return err
	}

	for _, ref := range refs {
		if !ref.Name().IsBranch() || ref.Type() != plumbing.HashReference {
			continue
		}

		name := plumbing.NewBranchReferenceName(ref.Name().Short())

		if current, err := repo.Storer.Reference(name); err == nil {
			// Determine whether the reference has changed to skip
			// unnecessary modifications.
			if current.Hash() == ref.Hash() {
				continue
			}
		}

		branch := plumbing.NewHashReference(name, ref.Hash())

		err := repo.Storer.SetReference(branch)
		if err != nil {
			return err
		}

		if ref.Name().Short() == "master" {
			go triggerBuild(ref.Hash().String())
		}

		log.Println("Updated branch", ref.Name().String())
	}

	return nil
}

func updateRepo(auth *githttp.BasicAuth, repo *git.Repository, opts *git.FetchOptions) error {
	err := repo.Fetch(opts)

	if err == git.NoErrAlreadyUpToDate {
		// nothing to do ...
		return nil
	} else if err != nil {
		return err
	}

	log.Println("Fetched updates from remote, updating local branches")
	return updateBranches(auth, repo)
}

func cloneRepo(dest, project, repo string, auth *githttp.BasicAuth) (*git.Repository, error) {
	var cloneOpts = git.CloneOptions{
		Auth: auth,
		URL:  fmt.Sprintf("https://source.developers.google.com/p/%s/r/%s", project, repo),
	}

	handle, err := git.PlainClone(dest, true, &cloneOpts)

	if err == git.ErrRepositoryAlreadyExists {
		handle, err = git.PlainOpen(dest)
	}

	return handle, updateBranches(auth, handle)
}

func main() {
	dest := EnvOr("SYNC_DEST", "/git/depot")
	project := EnvOr("SYNC_PROJECT", "tazjins-infrastructure")
	repo := EnvOr("SYNC_REPO", "depot")
	user := os.Getenv("SYNC_USER")
	pass := os.Getenv("SYNC_PASS")

	log.Printf("Syncing repository '%s/%s' to destination '%s'", project, repo, dest)

	var auth *githttp.BasicAuth
	if user != "" && pass != "" {
		auth = &githttp.BasicAuth{
			Username: user,
			Password: pass,
		}
		log.Println("Enabling basic authentication as user", user)
	}

	handle, err := cloneRepo(dest, project, repo, auth)

	if err != nil {
		log.Fatalf("Failed to clone repository: %s", err)
	} else {
		log.Println("Initiating update loop")
	}

	fetchOpts := git.FetchOptions{
		Auth:  auth,
		Force: true,
	}

	for {
		if err = updateRepo(auth, handle, &fetchOpts); err != nil {
			log.Fatalf("Failed to pull updated repository: %s", err)
		}

		time.Sleep(10 * time.Second)
	}
}
