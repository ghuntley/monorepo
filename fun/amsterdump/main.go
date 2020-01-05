// Amsterdump is a small program that populates a BigQuery table with
// a matrix of origin points scattered around Amsterdam and each
// respective points travel time to a given destination.
//
// The two destinations used here are the Schiphol Airport and
// Amsterdam Central station.
//
// To accomplish this the Google Maps Distance Matrix API [1] is
// queried with the points. A visualisation is later done using
// BigQuery GeoViz[2].
//
// [1]: https://developers.google.com/maps/documentation/distance-matrix/start#quotas
// [2]: https://bigquerygeoviz.appspot.com/
package main

import (
	"context"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"os"

	"googlemaps.github.io/maps"
)

func failOn(err error, msg string) {
	if err != nil {
		log.Fatalln(msg, err)
	}
}

type LocationResult struct {
	Address  string                      `json:"address"`
	URL      string                      `json:"url"`
	Schiphol *maps.DistanceMatrixElement `json:"schiphol"`
	Centraal *maps.DistanceMatrixElement `json:"centraal"`
}

type Listing struct {
	URL     string `json:"url"`
	Address string `json:"address"`
}

func requestMatrix(ctx context.Context, client *maps.Client, listings []Listing) {
	origins := make([]string, len(listings))
	for i, l := range listings {
		origins[i] = l.Address
	}

	request := &maps.DistanceMatrixRequest{
		Mode:    maps.TravelModeTransit,
		Units:   maps.UnitsMetric,
		Origins: origins,

		Destinations: []string{
			"Schiphol Airport",
			"Amsterdam Centraal",
		},
	}

	response, err := client.DistanceMatrix(ctx, request)
	failOn(err, "could not retrieve distance matrix:")

	for idx, addr := range response.OriginAddresses {
		result := LocationResult{
			Address:  addr,
			URL:      listings[idx].URL,
			Schiphol: response.Rows[idx].Elements[0],
			Centraal: response.Rows[idx].Elements[1],
		}

		j, _ := json.Marshal(result)
		fmt.Println(string(j))
	}
}

func main() {
	var listings []Listing
	input, err := ioutil.ReadFile("fun/amsterdump/input.json")
	failOn(err, "could not read input file:")

	err = json.Unmarshal(input, &listings)
	failOn(err, "could not deserialise listings:")

	ctx := context.Background()
	apiKey = os.Getenv("MAPS_API_KEY", "")
	if apiKey == "" {
		log.Fatalln("API key must be supplied via MAPS_API_KEY")
	}

	client, err := maps.NewClient(maps.WithAPIKey(apiKey))
	failOn(err, "could not create Google Maps API client:")

	var chunk []Listing
	for _, l := range listings {
		if len(chunk) == 25 {
			requestMatrix(ctx, client, chunk)
			chunk = []Listing{}
		} else {
			chunk = append(chunk, l)
		}
	}

	if len(chunk) > 1 {
		requestMatrix(ctx, client, chunk)
	}
}
