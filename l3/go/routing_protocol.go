package main

import (
	"fmt"
	"math"
	"math/rand"
	"sort"
)

const (
	n = 5
	d = 2
)

func exist(v1 int, v2 int, tab [d][2]int) bool {

	i := 0
	for i < d {
		if (tab[i][0] == v1 && tab[i][1] == v2) || (tab[i][0] == v2 && tab[i][1] == v1) {
			return true
		}
		i++
	}
	return false
}

func random_edges(additional *[]int) [d][2]int {

	var random_edges [d][2]int
	additional2 := make([]int, n)
	i := 0
	for i < d {
		vertex1 := rand.Intn(n)
		vertex2 := rand.Intn(n)
		if vertex1 != vertex2 && math.Abs(float64(vertex2-vertex1)) != 1 && exist(vertex1, vertex2, random_edges) != true {
			random_edges[i][0] = vertex1
			random_edges[i][1] = vertex2
			i++
			additional2[vertex1]++
			additional2[vertex2]++
		}
	}
	*additional = additional2
	return random_edges
}

func print_edges(edges [][]int) {
	for i := 0; i < n; i++ {
		sort.Ints(edges[i])
		for j := 0; j < len(edges[i]); j++ {
			fmt.Println(i, " - ", edges[i][j])
		}
	}
}

func main() {

	additional := make([]int, n)
	which := make([]int, n)
	var random_edges [d][2]int = random_edges(&additional)
	edges := make([][]int, n)
	for i := 0; i < n; i++ {
		if i == 0 {
			len := additional[i] + 1
			edges[i] = make([]int, len)
			edges[i][0] = i + 1
			which[i] = 1
		} else if i == n-1 {
			len := additional[i] + 1
			edges[i] = make([]int, len)
			edges[i][0] = i - 1
			which[i] = 1
		} else {
			len := additional[i] + 2
			edges[i] = make([]int, len)
			edges[i][0] = i - 1
			edges[i][1] = i + 1
			which[i] = 2
		}
	}
	for i := 0; i < d; i++ {
		from, to := random_edges[i][0], random_edges[i][1]
		edges[from][which[from]] = to
		which[from]++

		from2, to2 := random_edges[i][1], random_edges[i][0]
		edges[from2][which[from2]] = to2
		which[from2]++
	}
	print_edges(edges)
}
