Quinistry
=========

*A simple Docker registry quine.*

## What?

This is an example project for a from-scratch implementation of an HTTP server compatible with the [Docker Registry V2][]
protocol.

It serves a single image called `quinistry:latest` which is a Docker image that runs quinistry itself, therefore it is a
sort of Docker registry [quine][].

The official documentation does not contain enough information to actually implement this protocol (which I assume is
intentional), but a bit of trial&error lead there anyways. I've added comments to parts of the code to clear up things
that may be helpful to other developers in the future.

## Example

```
# Run quinistry:
vincent@urdhva ~/go/src/github.com/tazjin/quinistry (git)-[master] % ./quinistry
2017/03/16 14:11:56 Starting quinistry

# Pull the quinistry image from itself:
vincent@urdhva ~ % docker pull localhost:8080/quinistry
Using default tag: latest
latest: Pulling from quinistry
7bf1a8b18466: Already exists
Digest: sha256:d5cd4490901ef04b4e28e4ccc03a1d25fe3461200cf4d7166aab86fcd495e22e
Status: Downloaded newer image for localhost:8080/quinistry:latest

# Quinistry will log:
2017/03/16 14:14:03 Acknowleding V2 API: GET /v2/
2017/03/16 14:14:03 Serving manifest: GET /v2/quinistry/manifests/latest
2017/03/16 14:14:03 Serving config: GET /v2/quinistry/blobs/sha256:fbb165c48849de16017aa398aa9bb08fd1c00eaa7c150b6c2af37312913db279

# Run the downloaded image:
vincent@urdhva ~ % docker run -p 8090:8080 localhost:8080/quinistry
2017/03/16 13:15:18 Starting quinistry

# And download it again from itself:
vincent@urdhva ~ % docker pull localhost:8090/quinistry
Using default tag: latest
latest: Pulling from quinistry
7bf1a8b18466: Already exists
Digest: sha256:11141d95ddce0bac9ffa32ab1e8bc94748ed923e87762c68858dc41d11a46d3f
Status: Downloaded newer image for localhost:8090/quinistry:latest
```

## Building

Quinistry creates a Docker image that only contains a statically linked `main` binary. As this package makes use of
`net/http`, Go will (by default) link against `libc` for DNS resolution and create a dynamic binary instead.

To disable this, `build` the project with `-tags netgo`:

```
go build -tags netgo
```

[Docker Registry V2]: https://docs.docker.com/registry/spec/api/
[quine]: https://en.wikipedia.org/wiki/Quine_(computing)