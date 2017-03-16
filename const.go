package main

// HTTP content types

const ImageConfigMediaType string = "application/vnd.docker.container.image.v1+json"
const ManifestMediaType string = "application/vnd.docker.distribution.manifest.v2+json"
const LayerMediaType string = "application/vnd.docker.image.rootfs.diff.tar.gzip"

// HTTP header names

const ContentType string = "Content-Type"
const DigestHeader string = "Docker-Content-Digest"
