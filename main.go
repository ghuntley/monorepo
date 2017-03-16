package main

import (
	"fmt"
	"log"
	"net/http"
)

func main() {
	log.Println("Starting quinistry")

	image := GetImageOfCurrentExecutable()

	layerUri := fmt.Sprintf("/v2/quinistry/blobs/%s", image.LayerDigest)
	configUri := fmt.Sprintf("/v2/quinistry/blobs/%s", image.ConfigDigest)

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
			w.Header().Set(ContentType, ManifestMediaType)
			w.Header().Add(DigestHeader, image.ManifestDigest)
			w.Write(image.Manifest)
			return
		}

		// Serve actual image layer
		if r.RequestURI == layerUri {
			logRequest("Serving image layer blob", r)
			w.Header().Add(DigestHeader, image.LayerDigest)
			w.Write(image.Layer)
			return
		}

		// Serve image config
		if r.RequestURI == configUri {
			logRequest("Serving config", r)
			w.Header().Set("Content-Type", ImageConfigMediaType)
			w.Header().Set(DigestHeader, image.ConfigDigest)
			w.Write(image.Config)
			return
		}

		log.Printf("Unhandled request: %v\n", *r)
	})))
}

func logRequest(msg string, r *http.Request) {
	log.Printf("%s: %s %s\n", msg, r.Method, r.RequestURI)
}
