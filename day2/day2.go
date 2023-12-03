package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"
)

func part1(filename string) int {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()
	bag := map[string]int{
		"red": 12,
		"green": 13,
		"blue": 14,
	}
	re := regexp.MustCompile(`\s*(\d+) (\w+),?`)
	sum := 0

	i := 0
	scanner := bufio.NewScanner(file)
OUTER:
	for scanner.Scan() {
		i++
		useful := strings.Split(scanner.Text(), ":")[1]
		pulls := strings.Split(useful, ";")
		for _, pull := range pulls {
			cubes := re.FindAllStringSubmatch(pull, 3)
			for _, cube := range cubes {
				count, _ := strconv.Atoi(cube[1])
				if count > bag[cube[2]] {
					continue OUTER
				}
			}
		}
		sum += i
	}

	return sum
}

func part2(filename string) int {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()
	re := regexp.MustCompile(`\s*(\d+) (\w+),?`)
	sum := 0

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		reqs := map[string]int{
			"red": 0,
			"green": 0,
			"blue": 0,
		}
		useful := strings.Split(scanner.Text(), ":")[1]
		pulls := strings.Split(useful, ";")
		for _, pull := range pulls {
			cubes := re.FindAllStringSubmatch(pull, 3)
			for _, cube := range cubes {
				count, _ := strconv.Atoi(cube[1])
				if count > reqs[cube[2]] {
					reqs[cube[2]] = count
				}
			}
		}
		power := reqs["red"] * reqs["green"] * reqs["blue"]
		sum += power
	}

	return sum
}

func main() {
	fmt.Println(part1("input.txt"))
	fmt.Println(part2("input.txt"))
}