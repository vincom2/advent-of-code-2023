package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sort"
	"strconv"
	"strings"
)

type Card int

const (
	Joker Card = iota
	Two
	Three
	Four
	Five
	Six
	Seven
	Eight
	Nine
	Ten
	Queen
	King
	Ace
)

func CharToCard(c rune) Card {
	switch c {
	case '2':
		return Two
	case '3':
		return Three
	case '4':
		return Four
	case '5':
		return Five
	case '6':
		return Six
	case '7':
		return Seven
	case '8':
		return Eight
	case '9':
		return Nine
	case 'T':
		return Ten
	case 'J':
		return Joker
	case 'Q':
		return Queen
	case 'K':
		return King
	case 'A':
		return Ace
	default:
		panic("wtf")
	}
}

type HandStrength int

const (
	HighCard HandStrength = iota
	OnePair
	TwoPair
	ThreeOfAKind
	FullHouse
	FourOfAKind
	FiveOfAKind
)

type Hand struct {
	orig string
	hand []Card

	evaluated bool
	strength  HandStrength
}

func NewHand(orig string) Hand {
	hand := make([]Card, 0, 5)
	for _, c := range orig {
		hand = append(hand, CharToCard(c))
	}

	return Hand{orig: orig, hand: hand}
}

func (h Hand) Strength() HandStrength {
	if h.evaluated {
		return h.strength
	}

	contents := make(map[Card]int, len(h.hand))
	var joker_count int
	for _, c := range h.hand {
		if c == Joker {
			joker_count++
		} else {
			contents[c] += 1
		}
	}
	values := make([]int, len(contents))
	for _, v := range contents {
		values = append(values, v)
	}
	sort.Slice(values, func(i, j int) bool { return values[i] > values[j] })

	if len(values) == 0 { // JJJJJ
		values = append(values, 0)
	}
	values[0] += joker_count
	switch values[0] {
	case 5:
		h.strength = FiveOfAKind
	case 4:
		h.strength = FourOfAKind
	case 3:
		if values[1] == 2 {
			h.strength = FullHouse
		} else {
			h.strength = ThreeOfAKind
		}
	case 2:
		if values[1] == 2 {
			h.strength = TwoPair
		} else {
			h.strength = OnePair
		}
	default:
		h.strength = HighCard
	}

	h.evaluated = true
	return h.strength
}

func (h1 Hand) WeakerThan(h2 Hand) bool {
	strength1 := h1.Strength()
	strength2 := h2.Strength()

	if strength1 == strength2 {
		for i, c1 := range h1.hand {
			c2 := h2.hand[i]
			if c1 == c2 {
				continue
			}
			return c1 < c2
		}

		return false
	}

	return strength1 < strength2
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	var hands []Hand
	hands_to_bids := make(map[string]int)
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		contents := strings.Fields(scanner.Text())
		hands = append(hands, NewHand(contents[0]))
		bid, _ := strconv.Atoi(contents[1])
		hands_to_bids[contents[0]] = bid
	}

	sort.Slice(hands, func(i, j int) bool { return hands[i].WeakerThan(hands[j]) })

	winnings := 0
	for i, hand := range hands {
		winning := (i + 1) * hands_to_bids[hand.orig]
		winnings += winning
	}

	fmt.Println(winnings)
}
