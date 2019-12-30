// Copyright 2019 Google LLC.
// SPDX-License-Identifier: Apache-2.0
//
// sync-gcsr implements a small utility that periodically mirrors a
// remote Google Cloud Source Repository to a local file path.
package main

import (
	"fmt"
	"log"
	"os"
	"time"

	git "gopkg.in/src-d/go-git.v4"
	"gopkg.in/src-d/go-git.v4/plumbing"
	"gopkg.in/src-d/go-git.v4/plumbing/transport/http"
)

func EnvOr(key, def string) string {
	v := os.Getenv(key)
	if v == "" {
		return def
	}

	return v
}

// ensure that all remote branches exist locally & are up to date.
func updateBranches(auth *http.BasicAuth, repo *git.Repository) error {
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

		branch := plumbing.NewHashReference(
			plumbing.NewBranchReferenceName(ref.Name().Short()),
			ref.Hash(),
		)

		err := repo.Storer.SetReference(branch)
		if err != nil {
			return err
		}
		log.Println("Updated branch", ref.Name().String())
	}

	return nil
}

func updateRepo(auth *http.BasicAuth, repo *git.Repository, opts *git.FetchOptions) error {
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

func cloneRepo(dest, project, repo string, auth *http.BasicAuth) (*git.Repository, error) {
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

	var auth *http.BasicAuth
	if user != "" && pass != "" {
		auth = &http.BasicAuth{
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
