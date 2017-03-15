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
)

type Layer struct {
	MediaType string `json:"mediaType"`
	Size      int    `json:"size"`
	Digest    string `json:"digest"`
}

type Manifest struct {
	SchemaVersion int               `json:"schemaVersion"`
	MediaType     string            `json:"mediaType"`
	Config        map[string]string `json:"config"`
	Layers        []Layer           `json:"layers"`
}

// A really "dumb" representation of an image, with a data blob (tar.gz image) and its hash as the type expected
// in the manifest.
type Image struct {
	Layer Layer
	Data  []byte
}

const ManifestContentType string = "application/vnd.docker.distribution.manifest.v2+json"
const LayerContentType string = "application/vnd.docker.image.rootfs.diff.tar.gzip"

func main() {
	img := getImage()

	manifest := Manifest{
		SchemaVersion: 2,
		MediaType:     "application/vnd.docker.distribution.manifest.v2+json",
		Config:        map[string]string{},
		Layers: []Layer{
			img.Layer,
		},
	}

	log.Fatal(http.ListenAndServe(":8080", http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		// Acknowledge that we speak V2
		if r.RequestURI == "/v2/" {
			log.Println("Acknowleding V2 API")
			fmt.Fprintln(w)
			return
		}

		// Serve manifest
		if r.RequestURI == "/v2/quinistry/manifests/latest" {
			log.Printf("Serving manifest for %v\n", *r)
			w.Header().Add("Content-Type", ManifestContentType)
			resp, _ := json.Marshal(manifest)
			w.Header().Add("Docker-Content-Digest", digest(resp))
			log.Println(digest(resp))
			fmt.Fprintln(w, string(resp))
			return
		}

		// Serve actual image layer
		if r.RequestURI == fmt.Sprintf("/v2/quinistry/blob/%s", img.Layer.Digest) {
			fmt.Printf("Serving layer for %v\n", *r)
			fmt.Fprint(w, img.Data)
			return
		}

		fmt.Printf("Unhandled: %v\n", *r)
	})))
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
		Name: "main",
		Mode: 0755,
		Size: int64(len(file)),
	}
	tarW.WriteHeader(hdr)
	tarW.Write(file)

	if err := tarW.Close(); err != nil {
		log.Fatalln(err)
		os.Exit(1)
	}

	// Then GZIP it
	zBuf := new(bytes.Buffer)
	zw := gzip.NewWriter(zBuf)
	zw.Name = "Docker registry fake test"

	zw.Write(tarBuf.Bytes())
	if err := zw.Close(); err != nil {
		log.Fatal(err)
		os.Exit(1)
	}

	return &Image{
		Layer: Layer{
			MediaType: LayerContentType,
			Size:      zBuf.Len(),
			Digest:    digest(zBuf.Bytes()),
		},
		Data: zBuf.Bytes(),
	}
}
