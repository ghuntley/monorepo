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
	handle, err := git.PlainClone(dest, true, &cloneOpts)

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

	fetchOpts := git.FetchOptions{
		Auth: cloneOpts.Auth,
	}

	for {
		time.Sleep(30 * time.Second) //  TODO(tazjin): Config option for fetch interval?
		err = handle.Fetch(&fetchOpts)

		if err == git.NoErrAlreadyUpToDate {
			// no-op ...
			err = nil
		} else if err != nil {
			log.Fatalf("Failed to fetch updated repository: %s", err)
		} else {
			log.Println("Fetched new updates from remote repository")
		}
	}
}
