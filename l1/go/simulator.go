package main

import (
	"fmt"
	"math/rand"
	"sort"
	"strconv"
	"time"
)

const (
	n = 5
	d = 2
	k = 3
)

type vertex struct {
	id          int
	packages    []int
	where_to_go []int
	empty       bool
}

type packagee struct {
	id               int
	visited_vertices []int
}

func exist(v1 int, v2 int, tab [d][2]int) bool {

	i := 0
	for i < d {
		if tab[i][0] == v1 && tab[i][1] == v2 {
			return true
		}
		i++
	}
	return false
}

func random_edges(additional *[]int) [d][2]int {

	var random_edges [d][2]int
	additional2 := make([]int, n-1)
	i := 0
	for i < d {
		vertex1 := rand.Intn(n)
		vertex2 := rand.Intn(n - 1)
		if vertex1 != vertex2 && vertex2 != vertex1+1 && vertex1 < vertex2 && exist(vertex1, vertex2, random_edges) != true && vertex1 != n-1 {
			random_edges[i][0] = vertex1
			random_edges[i][1] = vertex2
			i++
			additional2[vertex1]++
		}
	}
	*additional = additional2
	return random_edges
}

func print_edges(edges [][]int) {
	for i := 0; i < n-1; i++ {
		sort.Ints(edges[i])
		for j := 0; j < len(edges[i]); j++ {
			fmt.Println(i, " -> ", edges[i][j])
		}
	}
}

func printing(printer <-chan string, finished chan bool, finished2 chan bool) {

	for {
		select {
		case message := <-printer:
			fmt.Println(message)
		case done := <-finished:
			finished2 <- done
		}
	}
}

func vertex_routine(v *vertex, chans [n]chan *packagee, printer chan<- string) {

	var p *packagee
	for {
		if v.empty == true {
			select {
			case pack := <-chans[v.id]:
				p = pack
				msg := "pakiet " + strconv.Itoa(pack.id) + " jest w wierzcholku " + strconv.Itoa(v.id)
				pack.visited_vertices = append(pack.visited_vertices, v.id)
				v.packages = append(v.packages, pack.id)
				v.empty = false
				printer <- msg
			}
		}

		if v.empty == false {
			time.Sleep(1 * time.Second)
			l := len(v.where_to_go)
			rand := rand.Intn(l)
			x := v.where_to_go[rand]
			channel := chans[x]
			channel <- p
			v.empty = true
		}
		time.Sleep(1 * time.Second)
	}

}

func receiver(pickup <-chan *packagee, printer chan<- string, finished chan bool) {

	counter := 0
	for {
		pack, ok := <-pickup
		if ok {
			msg := "pakiet " + strconv.Itoa(pack.id) + " zostal odebrany"
			printer <- msg
			counter++
			if counter == k {
				finished <- true
			}
		}
		time.Sleep(1 * time.Second)
	}
}

func sender(pack []*packagee, to0 chan<- *packagee) {

	for i := 0; i < k; i++ {
		to0 <- pack[i]
		time.Sleep(2 * time.Second)
	}
}

func destiny(v *vertex, chans [n]chan *packagee, pickup chan<- *packagee, printer chan<- string) {

	var p *packagee
	for {

		if v.empty == true {
			select {
			case pack := <-chans[n-1]:
				p = pack
				msg := "pakiet " + strconv.Itoa(pack.id) + " jest w wierzcholku " + strconv.Itoa(v.id)
				pack.visited_vertices = append(pack.visited_vertices, v.id)
				v.packages = append(v.packages, pack.id)
				v.empty = false
				printer <- msg
			}
		}

		if v.empty == false {
			time.Sleep(1 * time.Second)
			pickup <- p
			v.empty = true
		}
		time.Sleep(1 * time.Second)

	}

}

func statistics(v []*vertex, p []*packagee) {
	fmt.Println()
	fmt.Println("Obsluzone pakiety:")
	for i := 0; i < n; i++ {
		fmt.Print("Wierzcholek " + strconv.Itoa(v[i].id) + " : ")
		for j := 0; j < len(v[i].packages); j++ {
			fmt.Print(strconv.Itoa(v[i].packages[j]) + " ")
		}
		fmt.Println()
	}

	fmt.Println()
	fmt.Println("Odwiedzone wierzcholki:")
	for i := 0; i < k; i++ {
		fmt.Print("Pakiet " + strconv.Itoa(p[i].id) + " : ")
		for j := 0; j < len(p[i].visited_vertices); j++ {
			fmt.Print(strconv.Itoa(p[i].visited_vertices[j]) + " ")
		}
		fmt.Println()
	}
}

func main() {

	finished := make(chan bool)
	finished2 := make(chan bool)

	v := make([]*vertex, n)
	p := make([]*packagee, k)

	var vis []int
	for i := 0; i < k; i++ {
		pack := packagee{id: i, visited_vertices: vis}
		p[i] = &pack
	}

	additional := make([]int, n-1)
	which := make([]int, n-1)
	var random_edges [d][2]int = random_edges(&additional)
	edges := make([][]int, n-1)
	for i := 0; i < n-1; i++ {
		len := additional[i] + 1
		edges[i] = make([]int, len)
		edges[i][0] = i + 1
		which[i] = 1
	}
	for i := 0; i < d; i++ {
		from, to := random_edges[i][0], random_edges[i][1]
		edges[from][which[from]] = to
		which[from]++
	}
	print_edges(edges)

	printer := make(chan string, k*n)
	go printing(printer, finished, finished2)

	pickup := make(chan *packagee, k)

	var chans [n]chan *packagee
	for i := range chans {
		chans[i] = make(chan *packagee, k)
	}
	go sender(p, chans[0])

	var pac []int
	for i := 0; i < n-1; i++ {
		x := vertex{id: i, packages: pac, where_to_go: edges[i], empty: true}
		v[i] = &x
		go vertex_routine(&x, chans, printer)
	}
	x := vertex{id: n - 1, packages: pac, where_to_go: pac, empty: true}
	v[n-1] = &x
	go destiny(&x, chans, pickup, printer)

	go receiver(pickup, printer, finished)

	<-finished2
	statistics(v, p)
}
