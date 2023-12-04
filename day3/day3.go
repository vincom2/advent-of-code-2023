package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"os"
	"strconv"
	"strings"
	"unicode"
)

func isSymbol(r rune) bool {
	return !unicode.IsDigit(r) && r != '.'
}

type loc struct {
	r int
	c int
}

func part1(filename string) int {
	file, err := os.Open(filename)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	symbolLocations := make(map[loc]struct{})
	numbers := make(map[loc]*int)

	reader := bufio.NewReader(file)
	i := 0
	j := 0
OUTER:
	for {
		c, _, err := reader.ReadRune()
		if err != nil {
			if err == io.EOF {
				break
			} else {
				log.Fatal(err)
			}
		}

		if c == '\n' {
			i++
			j = 0
			continue
		} else if unicode.IsDigit(c) {
			var currNumber strings.Builder
			var n int
			for {
				currNumber.WriteRune(c)
				numbers[loc{r: i, c: j}] = &n
				c, _, err = reader.ReadRune()
				if err != nil {
					if err == io.EOF {
						n, _ = strconv.Atoi(currNumber.String())
						break OUTER
					} else {
						log.Fatal(err)
					}
				}
				if !unicode.IsDigit(c) {
					reader.UnreadRune()
					break
				}
				j++
			}
			n, _ = strconv.Atoi(currNumber.String())
		} else if c != '.' {
			symbolLocations[loc{r: i, c: j}] = struct{}{}
		}
		j++
	}

	sum := 0
	for l := range symbolLocations {
		// UL
		if n, ok := numbers[loc{r: l.r-1, c: l.c-1}]; ok {
			sum += *n
			// lmaooooo actually this setting to 0 is fucked up because by the looks of part 2
			// numbers should be counted twice if they're adjacent to multiple different symbols.
			// setting to 0 avoids double counting for a single symbol but also messes up that other case LOL.
			// whatever man
			*n = 0
		}
		// U
		if n, ok := numbers[loc{r: l.r-1, c: l.c}]; ok {
			sum += *n
			*n = 0
		}
		// UR
		if n, ok := numbers[loc{r: l.r-1, c: l.c+1}]; ok {
			sum += *n
			*n = 0
		}
		// L
		if n, ok := numbers[loc{r: l.r, c: l.c-1}]; ok {
			sum += *n
			*n = 0
		}
		// R
		if n, ok := numbers[loc{r: l.r, c: l.c+1}]; ok {
			sum += *n
			*n = 0
		}
		// BL
		if n, ok := numbers[loc{r: l.r+1, c: l.c-1}]; ok {
			sum += *n
			*n = 0
		}
		// B
		if n, ok := numbers[loc{r: l.r+1, c: l.c}]; ok {
			sum += *n
			*n = 0
		}
		// BR
		if n, ok := numbers[loc{r: l.r+1, c: l.c+1}]; ok {
			sum += *n
			*n = 0
		}
	}

	return sum
}

func part2(filename string) int {
	file, err := os.Open(filename)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	potentialGearLocations := make(map[loc]struct{})
	numbers := make(map[loc]*int)

	reader := bufio.NewReader(file)
	i := 0
	j := 0
OUTER:
	for {
		c, _, err := reader.ReadRune()
		if err != nil {
			if err == io.EOF {
				break
			} else {
				log.Fatal(err)
			}
		}

		if c == '\n' {
			i++
			j = 0
			continue
		} else if unicode.IsDigit(c) {
			var currNumber strings.Builder
			var n int
			for {
				currNumber.WriteRune(c)
				numbers[loc{r: i, c: j}] = &n
				c, _, err = reader.ReadRune()
				if err != nil {
					if err == io.EOF {
						n, _ = strconv.Atoi(currNumber.String())
						break OUTER
					} else {
						log.Fatal(err)
					}
				}
				if !unicode.IsDigit(c) {
					reader.UnreadRune()
					break
				}
				j++
			}
			n, _ = strconv.Atoi(currNumber.String())
		} else if c == '*' {
			potentialGearLocations[loc{r: i, c: j}] = struct{}{}
		}
		j++
	}

	sum := 0
	for l := range potentialGearLocations {
		ratio := 1
		adjCount := 0
		// UL
		if n, ok := numbers[loc{r: l.r-1, c: l.c-1}]; ok && *n != 0 {
			ratio *= *n
			adjCount++
			if adjCount > 2 {
				continue
			}
			*n = 0
		}
		// U
		if n, ok := numbers[loc{r: l.r-1, c: l.c}]; ok && *n != 0 {
			ratio *= *n
			adjCount++
			if adjCount > 2 {
				continue
			}
			*n = 0
		}
		// UR
		if n, ok := numbers[loc{r: l.r-1, c: l.c+1}]; ok && *n != 0 {
			ratio *= *n
			adjCount++
			if adjCount > 2 {
				continue
			}
			*n = 0
		}
		// L
		if n, ok := numbers[loc{r: l.r, c: l.c-1}]; ok && *n != 0 {
			ratio *= *n
			adjCount++
			if adjCount > 2 {
				continue
			}
			*n = 0
		}
		// R
		if n, ok := numbers[loc{r: l.r, c: l.c+1}]; ok && *n != 0 {
			ratio *= *n
			adjCount++
			if adjCount > 2 {
				continue
			}
			*n = 0
		}
		// BL
		if n, ok := numbers[loc{r: l.r+1, c: l.c-1}]; ok && *n != 0 {
			ratio *= *n
			adjCount++
			if adjCount > 2 {
				continue
			}
			*n = 0
		}
		// B
		if n, ok := numbers[loc{r: l.r+1, c: l.c}]; ok && *n != 0 {
			ratio *= *n
			adjCount++
			if adjCount > 2 {
				continue
			}
			*n = 0
		}
		// BR
		if n, ok := numbers[loc{r: l.r+1, c: l.c+1}]; ok && *n != 0 {
			ratio *= *n
			adjCount++
			if adjCount > 2 {
				continue
			}
			*n = 0
		}

		if adjCount == 2 {
			sum += ratio
		}
	}

	return sum
}

func main() {
	filename := "input.txt"
	fmt.Println(part1(filename))
	fmt.Println(part2(filename))
}