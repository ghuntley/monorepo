/*
Copyright 2015 Google Inc. All rights reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

// Package comment defines the internal representation of a review comment.
package comment

import (
	"crypto/sha1"
	"encoding/json"
	"errors"
	"fmt"
	"strconv"
	"strings"
	"time"

	"github.com/google/git-appraise/repository"
	"github.com/google/git-appraise/review/gpg"
)

// Ref defines the git-notes ref that we expect to contain review comments.
const Ref = "refs/notes/devtools/discuss"

// FormatVersion defines the latest version of the comment format supported by the tool.
const FormatVersion = 0

// ErrInvalidRange inidcates an error during parsing of a user-defined file
// range
var ErrInvalidRange = errors.New("invalid file location range. The required form is StartLine[+StartColumn][:EndLine[+EndColumn]]. The first line in a file is considered to be line 1")

// Range represents the range of text that is under discussion.
type Range struct {
	StartLine   uint32 `json:"startLine"`
	StartColumn uint32 `json:"startColumn,omitempty"`
	EndLine     uint32 `json:"endLine,omitempty"`
	EndColumn   uint32 `json:"endColumn,omitempty"`
}

// Location represents the location of a comment within a commit.
type Location struct {
	Commit string `json:"commit,omitempty"`
	// If the path is omitted, then the comment applies to the entire commit.
	Path string `json:"path,omitempty"`
	// If the range is omitted, then the location represents an entire file.
	Range *Range `json:"range,omitempty"`
}

// Check verifies that this location is valid in the provided
// repository.
func (location *Location) Check(repo repository.Repo) error {
	contents, err := repo.Show(location.Commit, location.Path)
	if err != nil {
		return err
	}
	lines := strings.Split(contents, "\n")
	if location.Range.StartLine > uint32(len(lines)) {
		return fmt.Errorf("Line number %d does not exist in file %q",
			location.Range.StartLine,
			location.Path)
	}
	if location.Range.StartColumn != 0 &&
		location.Range.StartColumn > uint32(len(lines[location.Range.StartLine-1])) {
		return fmt.Errorf("Line %d in %q is too short for column %d",
			location.Range.StartLine,
			location.Path,
			location.Range.StartColumn)
	}
	if location.Range.EndLine != 0 &&
		location.Range.EndLine > uint32(len(lines)) {
		return fmt.Errorf("End line number %d does not exist in file %q",
			location.Range.EndLine,
			location.Path)
	}
	if location.Range.EndColumn != 0 &&
		location.Range.EndColumn > uint32(len(lines[location.Range.EndLine-1])) {
		return fmt.Errorf("End line %d in %q is too short for column %d",
			location.Range.EndLine,
			location.Path,
			location.Range.EndColumn)
	}
	return nil
}

// Comment represents a review comment, and can occur in any of the following contexts:
// 1. As a comment on an entire commit.
// 2. As a comment about a specific file in a commit.
// 3. As a comment about a specific line in a commit.
// 4. As a response to another comment.
type Comment struct {
	// Timestamp and Author are optimizations that allows us to display comment threads
	// without having to run git-blame over the notes object. This is done because
	// git-blame will become more and more expensive as the number of code reviews grows.
	Timestamp string `json:"timestamp,omitempty"`
	Author    string `json:"author,omitempty"`
	// If original is provided, then the comment is an updated version of another comment.
	Original string `json:"original,omitempty"`
	// If parent is provided, then the comment is a response to another comment.
	Parent string `json:"parent,omitempty"`
	// If location is provided, then the comment is specific to that given location.
	Location    *Location `json:"location,omitempty"`
	Description string    `json:"description,omitempty"`
	// The resolved bit indicates that no further action is needed.
	//
	// When the parent of the comment is another comment, this means that comment
	// has been addressed. Otherwise, the parent is the commit, and this means that the
	// change has been accepted. If the resolved bit is unset, then the comment is only an FYI.
	Resolved *bool `json:"resolved,omitempty"`
	// Version represents the version of the metadata format.
	Version int `json:"v,omitempty"`

	gpg.Sig
}

// New returns a new comment with the given description message.
//
// The Timestamp and Author fields are automatically filled in with the current time and user.
func New(author string, description string) Comment {
	return Comment{
		Timestamp:   strconv.FormatInt(time.Now().Unix(), 10),
		Author:      author,
		Description: description,
	}
}

// Parse parses a review comment from a git note.
func Parse(note repository.Note) (Comment, error) {
	bytes := []byte(note)
	var comment Comment
	err := json.Unmarshal(bytes, &comment)
	return comment, err
}

// ParseAllValid takes collection of git notes and tries to parse a review
// comment from each one. Any notes that are not valid review comments get
// ignored, as we expect the git notes to be a heterogenous list, with only
// some of them being review comments.
func ParseAllValid(notes []repository.Note) map[string]Comment {
	comments := make(map[string]Comment)
	for _, note := range notes {
		comment, err := Parse(note)
		if err == nil && comment.Version == FormatVersion {
			hash, err := comment.Hash()
			if err == nil {
				comments[hash] = comment
			}
		}
	}
	return comments
}

func (comment Comment) serialize() ([]byte, error) {
	if len(comment.Timestamp) < 10 {
		// To make sure that timestamps from before 2001 appear in the correct
		// alphabetical order, we reformat the timestamp to be at least 10 characters
		// and zero-padded.
		time, err := strconv.ParseInt(comment.Timestamp, 10, 64)
		if err == nil {
			comment.Timestamp = fmt.Sprintf("%010d", time)
		}
		// We ignore the other case, as the comment timestamp is not in a format
		// we expected, so we should just leave it alone.
	}
	return json.Marshal(comment)
}

// Write writes a review comment as a JSON-formatted git note.
func (comment Comment) Write() (repository.Note, error) {
	bytes, err := comment.serialize()
	return repository.Note(bytes), err
}

// Hash returns the SHA1 hash of a review comment.
func (comment Comment) Hash() (string, error) {
	bytes, err := comment.serialize()
	return fmt.Sprintf("%x", sha1.Sum(bytes)), err
}

// Set implenents flag.Value for the Range type
func (r *Range) Set(s string) error {
	var err error
	*r = Range{}

	if s == "" {
		return nil
	}
	startEndParts := strings.Split(s, ":")
	if len(startEndParts) > 2 {
		return ErrInvalidRange
	}

	r.StartLine, r.StartColumn, err = parseRangePart(startEndParts[0])
	if err != nil {
		return err
	}
	if len(startEndParts) == 1 {
		return nil
	}

	r.EndLine, r.EndColumn, err = parseRangePart(startEndParts[1])
	if err != nil {
		return err
	}

	if r.StartLine > r.EndLine {
		return errors.New("start line cannot be greater than end line in range")
	}

	return nil
}

func parseRangePart(s string) (uint32, uint32, error) {
	parts := strings.Split(s, "+")
	if len(parts) > 2 {
		return 0, 0, ErrInvalidRange
	}

	line, err := strconv.ParseUint(parts[0], 10, 32)
	if err != nil {
		return 0, 0, ErrInvalidRange
	}

	if len(parts) == 1 {
		return uint32(line), 0, nil
	}

	col, err := strconv.ParseUint(parts[1], 10, 32)
	if err != nil {
		return 0, 0, ErrInvalidRange
	}

	if line == 0 && col != 0 {
		// line 0 represents the entire file
		return 0, 0, ErrInvalidRange
	}

	return uint32(line), uint32(col), nil
}

func (r *Range) String() string {
	out := ""
	if r.StartLine != 0 {
		out = fmt.Sprintf("%d", r.StartLine)
	}
	if r.StartColumn != 0 {
		out = fmt.Sprintf("%s+%d", out, r.StartColumn)
	}
	if r.EndLine != 0 {
		out = fmt.Sprintf("%s:%d", out, r.EndLine)
	}
	if r.EndColumn != 0 {
		out = fmt.Sprintf("%s+%d", out, r.EndColumn)
	}
	return out
}
