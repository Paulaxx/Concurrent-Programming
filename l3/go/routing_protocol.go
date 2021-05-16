package main

import (
	"fmt"
	"math"
	"math/rand"
	"sort"
	"time"
)

const (
	n = 5
	d = 2
)

type pair struct {
	verticle_id int
	cost        int
}

type packet struct {
	id      int
	channel chan []pair
}

type vertex struct {
	id          int
	where_to_go []int
}

type routing_table_item struct {
	nexthop int
	cost    int
	changed bool
}

type what_to_change struct {
	id   int
	item routing_table_item
}

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

func contains(neighbors []int, find int) bool {
	for _, element := range neighbors {
		if element == find {
			return true
		}
	}
	return false
}

func store_routing_table(id int, routing_table []routing_table_item, change_changed <-chan int, get_actual_routing_table <-chan int,
	routing_table_chan chan []routing_table_item, done chan bool, replacement <-chan what_to_change) {

	for {
		select {
		case j := <-change_changed:
			routing_table[j].changed = !routing_table[j].changed
			//done <- true
		case <-get_actual_routing_table:
			routing_table_chan <- routing_table
		case repl := <-replacement:
			routing_table[repl.id] = repl.item
			done <- true
		}
	}
}

func sender(v vertex, get_actual_routing_table chan int, routing_table_chan <-chan []routing_table_item, receivers []chan []pair, change_changed chan int) {

	for {
		get_actual_routing_table <- 0
		routing_table := <-routing_table_chan
		var packet []pair
		for id, element := range routing_table {
			if element.changed == true {
				pair := pair{verticle_id: id, cost: routing_table[id].cost}
				packet = append(packet, pair)
				change_changed <- id
			}
		}
		for _, element := range v.where_to_go {
			receivers[element] <- packet
		}
		time.Sleep(2000 * time.Millisecond)
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

	// kanaly do wysylania pakietow do receivera i odbierania tych pakietow przez receivera
	var receiver [n]chan []pair
	for i := range receiver {
		receiver[i] = make(chan []pair, n*n)
	}

	// kanaly do wysylania aktualnego routing table
	var return_actual_routing_table [n]chan []routing_table_item
	for i := range return_actual_routing_table {
		return_actual_routing_table[i] = make(chan []routing_table_item, n*n)
	}

	// kanaly do zmiany parametru changed na otrzymanej pozycji
	var change_changed [n]chan int
	for i := range change_changed {
		change_changed[i] = make(chan int, n*n)
	}

	// kanaly do wyslania zapytania o aktualna routing table
	var get_actual_routing_table [n]chan int
	for i := range get_actual_routing_table {
		get_actual_routing_table[i] = make(chan int, n*n)
	}

	var done [n]chan bool
	for i := range done {
		done[i] = make(chan bool, n*n)
	}

	var channel_to_change [n]chan what_to_change
	for i := range channel_to_change {
		channel_to_change[i] = make(chan what_to_change, n*n)
	}

	var nexthop, cost int
	var changed bool
	for i := 0; i < n; i++ {
		routing_table := make([]routing_table_item, n)
		for j := 0; j < n; j++ {
			if i != j {
				if contains(edges[i], j) {
					nexthop = j
					cost = 1
				} else {
					if i < j {
						nexthop = i + 1
						cost = j - i
					} else {
						nexthop = i - 1
						cost = i - j
					}
				}
				changed = true
				item := routing_table_item{nexthop: nexthop, cost: cost, changed: changed}
				routing_table[j] = item
			}
		}
		go store_routing_table(i, routing_table, change_changed[i], get_actual_routing_table[i], return_actual_routing_table[i], done[i], channel_to_change[i])
	}

}
