package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func part1(filename string) int {
	file, err := os.Open(filename)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var times []int
	var distances []int
	for scanner.Scan() {
		contents := strings.Split(scanner.Text(), ":")
		if contents[0] == "Time" {
			for _, n := range strings.Fields(contents[1]) {
				n_, _ := strconv.Atoi(n)
				times = append(times, n_)
			}
		} else {
			for _, n := range strings.Fields(contents[1]) {
				n_, _ := strconv.Atoi(n)
				distances = append(distances, n_)
			}
		}
	}

	var ways []int
	for i, time := range times {
		way := 0
		best_distance := distances[i]
		for t := 0; t <= time; t++ {
			travel_time := time - t
			distance := travel_time * t
			if distance > best_distance {
				way++
			}
			// you can break early but do i care? no
		}
		ways = append(ways, way)
	}

	margin := 1
	for _, way := range ways {
		margin *= way
	}

	return margin
}

func part2(filename string) int {
	file, err := os.Open(filename)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var time int
	var best_distance int
	for scanner.Scan() {
		contents := strings.Split(scanner.Text(), ":")
		if contents[0] == "Time" {
			time, _ = strconv.Atoi(strings.Join(strings.Fields(contents[1]), ""))
		} else {
			best_distance, _ = strconv.Atoi(strings.Join(strings.Fields(contents[1]), ""))
		}
	}

	var ways int
	for t := 0; t <= time; t++ {
		travel_time := time - t
		distance := travel_time * t
		if distance > best_distance {
			ways++
		}
	}

	return ways
}

func main() {
	filename := "input.txt"

	fmt.Println(part1(filename))
	fmt.Println(part2(filename))
}
