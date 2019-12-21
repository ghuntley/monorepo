// The code in this file creates a Docker image layer containing the binary of the
// application itself.

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
	"os"
	"time"
)

// This function creates a Docker-image digest (i.e. SHA256 hash with
// algorithm-specification prefix)
func Digest(b []byte) string {
	hash := sha256.New()
	hash.Write(b)

	return fmt.Sprintf("sha256:%x", hash.Sum(nil))
}

func GetImageOfCurrentExecutable() Image {
	binary := getCurrentBinary()
	tarArchive := createTarArchive(&map[string][]byte{
		"/main": binary,
	})

	configJson, configElem := createConfig([]string{Digest(tarArchive)})
	compressed := gzipArchive("Quinistry image", tarArchive)
	manifest := createManifest(&configElem, &compressed)
	manifestJson, _ := json.Marshal(manifest)

	return Image{
		Layer:          compressed,
		LayerDigest:    Digest(compressed),
		Manifest:       manifestJson,
		ManifestDigest: Digest(manifestJson),
		Config:         configJson,
		ConfigDigest:   Digest(configJson),
	}

}

func getCurrentBinary() []byte {
	path, _ := os.Executable()
	file, _ := ioutil.ReadFile(path)
	return file
}

func createTarArchive(files *map[string][]byte) []byte {
	buf := new(bytes.Buffer)
	w := tar.NewWriter(buf)

	for name, file := range *files {
		hdr := &tar.Header{
			Name: name,
			// Everything is executable \o/
			Mode: 0755,
			Size: int64(len(file)),
		}
		w.WriteHeader(hdr)
		w.Write(file)
	}

	if err := w.Close(); err != nil {
		log.Fatalln(err)
		os.Exit(1)
	}

	return buf.Bytes()
}

func gzipArchive(name string, archive []byte) []byte {
	buf := new(bytes.Buffer)
	w := gzip.NewWriter(buf)
	w.Name = name
	w.Write(archive)

	if err := w.Close(); err != nil {
		log.Fatalln(err)
		os.Exit(1)
	}

	return buf.Bytes()
}

func createConfig(layerDigests []string) (configJson []byte, elem Element) {
	now := time.Now()

	imageConfig := &ImageConfig{
		Cmd: []string{"/main"},
		Env: []string{"PATH=/"},
	}

	rootFs := RootFs{
		DiffIds: layerDigests,
		Type:    "layers",
	}

	history := []History{
		{
			Created:   now,
			CreatedBy: "Quinistry magic",
		},
	}

	config := Config{
		Created:      now,
		Author:       "tazjin",
		Architecture: "amd64",
		Os:           "linux",
		Config:       imageConfig,
		RootFs:       rootFs,
		History:      history,
	}

	configJson, _ = json.Marshal(config)

	elem = Element{
		MediaType: ImageConfigMediaType,
		Size:      len(configJson),
		Digest:    Digest(configJson),
	}

	return
}

func createManifest(config *Element, layer *[]byte) Manifest {
	layers := []Element{
		{
			MediaType: LayerMediaType,
			Size:      len(*layer),
			// Layers must contain the digest of the *gzipped* layer.
			Digest: Digest(*layer),
		},
	}

	return Manifest{
		SchemaVersion: 2,
		MediaType:     ManifestMediaType,
		Config:        *config,
		Layers:        layers,
	}
}
