package main

import "time"

// This type represents the rootfs-key of the Docker image config.
// It specifies the digest (i.e. usually SHA256 hash) of the tar'ed, but NOT
// compressed image layers.
type RootFs struct {
	// The digests of the non-compressed FS layers.
	DiffIds []string `json:"diff_ids"`

	// Type should always be set to "layers"
	Type string `json:"type"`
}

// This type represents an entry in the Docker image config's history key.
// Every history element "belongs" to a filesystem layer.
type History struct {
	Created   time.Time `json:"created"`
	CreatedBy string    `json:"created_by"`
}

// This type represents runtime-configuration for the Docker image.
// A lot of possible keys are omitted here, see:
// https://github.com/docker/docker/blob/master/image/spec/v1.2.md#image-json-description
type ImageConfig struct {
	Cmd []string
	Env []string
}

// This type represents the Docker image configuration
type Config struct {
	Created time.Time `json:"created"`
	Author  string    `json:"author"`

	// Architecture should be "amd64"
	Architecture string `json:"architecture"`

	// OS should be "linux"
	Os string `json:"os"`

	// Configuration can be set to 'nil', in which case all options have to be
	// supplied at container launch time.
	Config *ImageConfig `json:"config"`

	// Filesystem layers and history elements have to be in the same order.
	RootFs  RootFs    `json:"rootfs"`
	History []History `json:"history"`
}

// This type represents any manifest
type Element struct {
	MediaType string `json:"mediaType"`
	Size      int    `json:"size"`
	Digest    string `json:"digest"`
}

// This type represents a Docker image manifest as used by the registry
// protocol V2.
type Manifest struct {
	SchemaVersion int       `json:"schemaVersion"` // Must be 2
	MediaType     string    `json:"mediaType"`     // Use ManifestMediaType const
	Config        Element   `json:"config"`
	Layers        []Element `json:"layers"`
}

// A really "dumb" representation of an image, with its data blob and related
// metadata.
// Note: This is not a registry API type.
type Image struct {
	Layer       []byte
	LayerDigest string

	Manifest       []byte
	ManifestDigest string

	Config       []byte
	ConfigDigest string
}
