package main

import (
	"archive/tar"
	"bytes"
	"compress/gzip"
	"crypto/sha256"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"time"
)

const ImageContentType string = "application/vnd.docker.container.image.v1+json"
const ManifestContentType string = "application/vnd.docker.distribution.manifest.v2+json"
const LayerContentType string = "application/vnd.docker.image.rootfs.diff.tar.gzip"
const DigestHeader string = "Docker-Content-Digest"

type Element struct {
	MediaType string `json:"mediaType"`
	Size      int    `json:"size"`
	Digest    string `json:"digest"`
}

type Manifest struct {
	SchemaVersion int       `json:"schemaVersion"`
	MediaType     string    `json:"mediaType"`
	Config        Element   `json:"config"`
	Layers        []Element `json:"layers"`
}

// A really "dumb" representation of an image, with a data blob (tar.gz image) and its hash as the type expected
// in the manifest.
type Image struct {
	Data       []byte
	TarDigest  string
	GzipDigest string
}

func main() {
	log.Println("Starting quinistry")

	img := getImage()
	now := time.Now()

	config := Config{
		Created:      now,
		Author:       "tazjin",
		Architecture: "amd64",
		Os:           "linux",
		Config: &ImageConfig{
			Cmd: []string{"main"},
			Env: []string{"PATH=/"},
		},
		RootFs: RootFs{
			DiffIds: []string{
				img.TarDigest,
			},
			Type: "layers",
		},
		History: []History{
			{
				Created:   now,
				CreatedBy: "quinistry magic",
			},
		},
	}

	configJson, _ := json.Marshal(config)

	manifest := Manifest{
		SchemaVersion: 2,
		MediaType:     ManifestContentType,
		Config: Element{
			MediaType: ImageContentType,
			Size:      len(configJson),
			Digest:    digest(configJson),
		},
		Layers: []Element{
			{
				MediaType: LayerContentType,
				Size:      len(img.Data),
				Digest:    img.GzipDigest,
			},
		},
	}

	log.Fatal(http.ListenAndServe(":8080", http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		// Acknowledge that we speak V2
		if r.RequestURI == "/v2/" {
			logRequest("Acknowleding V2 API", r)
			fmt.Fprintln(w)
			return
		}

		// Serve manifest
		if r.RequestURI == "/v2/quinistry/manifests/latest" {
			logRequest("Serving manifest", r)
			w.Header().Add("Content-Type", ManifestContentType)
			resp, _ := json.Marshal(manifest)
			w.Header().Add(DigestHeader, digest(resp))
			w.Write(resp)
			return
		}

		// Serve actual image layer
		layerUri := fmt.Sprintf("/v2/quinistry/blobs/%s", img.GzipDigest)
		if r.RequestURI == layerUri {
			logRequest("Serving image layer blob", r)
			w.Header().Add(DigestHeader, img.GzipDigest)
			w.Write(img.Data)
			return
		}

		// Serve image config
		configUri := fmt.Sprintf("/v2/quinistry/blobs/%s", digest(configJson))
		if r.RequestURI == configUri {
			logRequest("Serving config", r)
			w.Header().Set("Content-Type", ImageContentType)
			w.Header().Set(DigestHeader, digest(configJson))
			w.Write(configJson)
			return
		}

		log.Printf("Unhandled request: %v\n", *r)
	})))
}

func logRequest(msg string, r *http.Request) {
	log.Printf("%s: %s %s\n", msg, r.Method, r.RequestURI)
}

func digest(b []byte) string {
	hash := sha256.New()
	hash.Write(b)

	return fmt.Sprintf("sha256:%x", hash.Sum(nil))
}

// Creates an image of the currently running binary (spooky!)
func getImage() *Image {
	// Current binary, imagine this is some other output or whatever
	path, _ := os.Executable()

	// don't care about error! :O
	file, _ := ioutil.ReadFile(path)

	// First create tar archive
	tarBuf := new(bytes.Buffer)
	tarW := tar.NewWriter(tarBuf)
	hdr := &tar.Header{
		Name: "/main",
		Mode: 0755,
		Size: int64(len(file)),
	}
	tarW.WriteHeader(hdr)
	tarW.Write(file)

	if err := tarW.Close(); err != nil {
		log.Fatalln(err)
		os.Exit(1)
	}

	tarBytes := tarBuf.Bytes()

	// Then GZIP it
	zBuf := new(bytes.Buffer)
	zw := gzip.NewWriter(zBuf)
	zw.Name = "Docker registry fake test"

	zw.Write(tarBytes)
	if err := zw.Close(); err != nil {
		log.Fatal(err)
		os.Exit(1)
	}

	gzipData := zBuf.Bytes()

	return &Image{
		TarDigest:  digest(tarBytes),
		GzipDigest: digest(gzipData),
		Data:       gzipData,
	}
}
