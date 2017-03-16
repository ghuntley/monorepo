package main

import "time"

type RootFs struct {
	DiffIds []string `json:"diff_ids"`
	Type    string   `json:"type"`
}

type History struct {
	Created   time.Time `json:"created"`
	CreatedBy string    `json:"created_by"`
}

type ImageConfig struct {
	Cmd []string
	Env []string
}

type Config struct {
	Created      time.Time    `json:"created"`
	Author       string       `json:"author"`
	Architecture string       `json:"architecture"`
	Os           string       `json:"os"`
	Config       *ImageConfig `json:"config"`
	RootFs       RootFs       `json:"rootfs"`
	History      []History    `json:"history"`
}
