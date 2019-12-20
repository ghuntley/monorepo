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
	"gopkg.in/src-d/go-git.v4/plumbing/transport/http"
)

func EnvOr(key, def string) string {
	v := os.Getenv(key)
	if v == "" {
		return def
	}

	return v
}

func updateRepo(repo *git.Repository, tree *git.Worktree, opts *git.PullOptions) error {
	err := tree.Pull(opts)
	if err == git.NoErrAlreadyUpToDate {
		// nothing to do ...
		return nil
	} else if err != nil {
		return err
	}

	log.Println("Updated local repository mirror")
	return nil
}

func main() {
	var dest = EnvOr("SYNC_DEST", "/git/depot")
	var project = EnvOr("SYNC_PROJECT", "tazjins-infrastructure")
	var repo = EnvOr("SYNC_REPO", "depot")
	var user = os.Getenv("SYNC_USER")
	var pass = os.Getenv("SYNC_PASS")

	log.Printf("Syncing repository '%s/%s' to destination '%s'", project, repo, dest)

	var cloneOpts = git.CloneOptions{
		URL: fmt.Sprintf("https://source.developers.google.com/p/%s/r/%s", project, repo),
	}

	if user != "" && pass != "" {
		cloneOpts.Auth = &http.BasicAuth{
			Username: user,
			Password: pass,
		}
		log.Println("Enabling basic authentication as user", user)
	}

	action := "clone"
	handle, err := git.PlainClone(dest, false, &cloneOpts)

	if err == git.ErrRepositoryAlreadyExists {
		log.Println("Repository has already been cloned!")
		handle, err = git.PlainOpen(dest)
		action = "open"
	}

	if err != nil {
		log.Fatalln("Failed to %s repository:", action, err)
	} else {
		log.Println("Initiating update loop")
	}

	tree, err := handle.Worktree()
	if err != nil {
		log.Fatalln("Failed to open repository worktree:", err)
	}

	pullOpts := git.PullOptions{
		Auth:  cloneOpts.Auth,
		Force: true,
	}

	for {
		if err = updateRepo(handle, tree, &pullOpts); err != nil {
			log.Fatalf("Failed to pull updated repository: %s", err)
		}
		time.Sleep(30 * time.Second) //  TODO(tazjin): Config option for pull interval?
	}
}
