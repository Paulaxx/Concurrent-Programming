package main

import (
	"fmt"
	"math"
	"math/rand"
	"os"
	"os/signal"
	"sort"
	"syscall"
	"time"
)

var finished chan bool

const (
	n = 5
	d = 2
	h = 3
)

type pair struct {
	verticle_id int
	cost        int
}

type pack struct {
	id     int
	packet []pair
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

type host struct {
	h int
	r int
}

type standard_packet struct {
	sender          host
	receiver        host
	visited_routers []int
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

func printing(printer <-chan string) {

	for {
		select {
		case message := <-printer:
			fmt.Println(message)
		}
	}
}

func receiver_queue(queue []standard_packet, return_first_element chan<- standard_packet, get_first_element chan int, add_to_queue chan standard_packet, verticle_id int) {

	for {
		select {
		case <-get_first_element:
			if len(queue) > 0 {
				return_first_element <- queue[0]
				queue = queue[1:]
			} else {
				return_first_element <- standard_packet{sender: host{-1, -1}, receiver: host{-1, -1}, visited_routers: []int{}}
			}
		case new_packet := <-add_to_queue:
			queue = append(queue, new_packet)
		}
	}
}

func receiverForwarder(verticle_id int, add_to_queue chan<- standard_packet, get_packet chan standard_packet) {

	for {
		select {
		case new_packet := <-get_packet:
			add_to_queue <- new_packet
		}
	}
}

func get_host_position(all_hosts [h]host, host_to_find host) int {
	for k, v := range all_hosts {
		if host_to_find == v {
			return k
		}
	}
	return -1
}

func receiverSender(verticle_id int, get_packet_from_queue chan standard_packet, send_ask_for_packet chan<- int, hosts [h]chan standard_packet, forwarders [n]chan standard_packet,
	routing_table_chan chan []routing_table_item, all_hosts [h]host) {

	for {
		send_ask_for_packet <- 1
		new_packet := <-get_packet_from_queue
		if new_packet.sender.r != -1 {
			new_packet.visited_routers = append(new_packet.visited_routers, verticle_id)
			if new_packet.receiver.r == verticle_id {
				hosts[get_host_position(all_hosts, host{h: new_packet.receiver.h, r: new_packet.receiver.r})] <- new_packet
			} else {
				routing_table := <-routing_table_chan
				var n int = routing_table[new_packet.receiver.r].nexthop
				forwarders[n] <- new_packet
			}
		}

	}
}

func host_func(r int, h int, forwarders [n]chan standard_packet, get_packet chan standard_packet, hosts [h]chan standard_packet, all_hosts [h]host, host_nb int) {

	var random_host int = rand.Intn(len(all_hosts))
	for random_host == host_nb {
		random_host = rand.Intn(len(all_hosts))
	}
	forwarders[r] <- standard_packet{sender: host{h: h, r: r}, receiver: host{h: all_hosts[random_host].h, r: all_hosts[random_host].r}, visited_routers: []int{}}
	for {
		new_packet := <-get_packet
		fmt.Println(new_packet)
		time.Sleep(2000 * time.Millisecond)
		forwarders[r] <- standard_packet{sender: host{h: h, r: r}, receiver: new_packet.sender, visited_routers: []int{}}
	}

}

func receiver_fun(to_receiver chan pack, routing_table_chan2 chan []routing_table_item, get_actual_routing_table2 chan<- int, replacement chan<- what_to_change) {

	for {
		select {
		case p := <-to_receiver:
			for _, p2 := range p.packet {
				new_cost := 1 + p2.cost
				get_actual_routing_table2 <- 0
				routing_table := <-routing_table_chan2
				if routing_table[p2.verticle_id].cost > new_cost {
					new_item := routing_table_item{nexthop: p.id, cost: new_cost, changed: true}
					to_change := what_to_change{id: p2.verticle_id, item: new_item}
					replacement <- to_change
				}
			}
		}
	}
}

func store_routing_table(id int, routing_table []routing_table_item, change_changed chan int, get_actual_routing_table chan int,
	routing_table_chan chan<- []routing_table_item, done chan<- bool, replacement chan what_to_change, get_actual_routing_table2 chan int,
	routing_table_chan2 chan<- []routing_table_item) {

	for {
		select {
		case j := <-change_changed:
			routing_table[j].changed = !routing_table[j].changed
		case <-get_actual_routing_table:
			routing_table_chan <- routing_table
		case <-get_actual_routing_table2:
			routing_table_chan2 <- routing_table
		case repl := <-replacement:
			routing_table[repl.id] = repl.item
			done <- true
		}
	}
}

func sender(v vertex, get_actual_routing_table chan<- int, routing_table_chan chan []routing_table_item, receivers [n]chan pack, change_changed chan<- int) {

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
		if len(packet) > 0 {
			for _, element := range v.where_to_go {
				pack := pack{id: v.id, packet: packet}
				receivers[element] <- pack
			}
		}
		time.Sleep(2000 * time.Millisecond)
	}
}

func Find(s [h]host, e host) bool {
	for _, a := range s {
		if a == e {
			return true
		}
	}
	return false
}

func generate_hosts() [h]host {

	all_hosts := [h]host{}
	how_many_hosts := make([]int, n)
	for i := 0; i < h; i++ {
		how_many_hosts[i] = 0
	}
	for i := 0; i < h; i++ {
		found := true
		r := 0
		h := 0
		for found {
			r = rand.Intn(n)
			h = how_many_hosts[r]
			found = Find(all_hosts, host{h: h, r: r})
		}
		how_many_hosts[r]++
		all_hosts[i] = host{h: h, r: r}
	}
	return all_hosts
}

func SetupCloseHandler() {
	c := make(chan os.Signal)
	signal.Notify(c, os.Interrupt, syscall.SIGTERM)
	go func() {
		<-c
		finished <- true
		//os.Exit(0)
	}()
}

func main() {

	SetupCloseHandler()

	finished = make(chan bool)

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

	all_hosts := generate_hosts()

	// kanaly do wysylania pakietow do receivera i odbierania tych pakietow przez receivera
	var receiver [n]chan pack
	for i := range receiver {
		receiver[i] = make(chan pack, n*n)
	}

	// kanaly do wysylania aktualnego routing table dla sendera
	var return_actual_routing_table [n]chan []routing_table_item
	for i := range return_actual_routing_table {
		return_actual_routing_table[i] = make(chan []routing_table_item, n*n)
	}

	// kanaly do wysylania aktualnego routing table dla receivera
	var return_actual_routing_table2 [n]chan []routing_table_item
	for i := range return_actual_routing_table2 {
		return_actual_routing_table2[i] = make(chan []routing_table_item, n*n)
	}

	// kanaly do zmiany parametru changed na otrzymanej pozycji
	var change_changed [n]chan int
	for i := range change_changed {
		change_changed[i] = make(chan int, n*n)
	}

	// kanaly do wyslania zapytania o aktualna routing table dla sendera'a
	var get_actual_routing_table [n]chan int
	for i := range get_actual_routing_table {
		get_actual_routing_table[i] = make(chan int, n*n)
	}

	// kanaly do wyslania zapytania o aktualna routing table dla receiver'a
	var get_actual_routing_table2 [n]chan int
	for i := range get_actual_routing_table2 {
		get_actual_routing_table2[i] = make(chan int, n*n)
	}

	// kanaly do wysylania pakietow do hostow
	var hosts_channels [h]chan standard_packet
	for i := range hosts_channels {
		hosts_channels[i] = make(chan standard_packet, h^3)
	}

	// kanaly do wysylania pakietow kolejki
	var send_packet_from_queue [n]chan standard_packet
	for i := range send_packet_from_queue {
		send_packet_from_queue[i] = make(chan standard_packet, n*n)
	}

	// kanaly do wysylania zapytania o pakiet z kolejki
	var packet_from_queue [n]chan int
	for i := range packet_from_queue {
		packet_from_queue[i] = make(chan int, n*n)
	}

	// kanaly do dodawania pakietu do kolejki
	// forwarder dodaje do kolejki
	var add_packet_to_queue [n]chan standard_packet
	for i := range add_packet_to_queue {
		add_packet_to_queue[i] = make(chan standard_packet, n*n)
	}

	// kanaly do wyslania pakietu do forwardera
	var send_to_receiver [n]chan standard_packet
	for i := range send_to_receiver {
		send_to_receiver[i] = make(chan standard_packet, n*n)
	}

	var done [n]chan bool
	for i := range done {
		done[i] = make(chan bool, n*n)
	}

	var channel_to_change [n]chan what_to_change
	for i := range channel_to_change {
		channel_to_change[i] = make(chan what_to_change, n*n)
	}

	printer := make(chan string, n*n)
	go printing(printer)

	var r_table [][]routing_table_item
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
		r_table = append(r_table, routing_table)
		go store_routing_table(i, routing_table, change_changed[i], get_actual_routing_table[i], return_actual_routing_table[i], done[i], channel_to_change[i],
			get_actual_routing_table2[i], return_actual_routing_table2[i])
	}

	for i := 0; i < n; i++ {
		x := vertex{id: i, where_to_go: edges[i]}
		go sender(x, get_actual_routing_table[i], return_actual_routing_table[i], receiver, change_changed[i])
		go receiver_fun(receiver[i], return_actual_routing_table2[i], get_actual_routing_table2[i], channel_to_change[i])
	}

	empty_slice := []standard_packet{}
	for i := 0; i < n; i++ {
		go receiver_queue(empty_slice, send_packet_from_queue[i], packet_from_queue[i], add_packet_to_queue[i], i)
	}

	for i := 0; i < n; i++ {
		go receiverForwarder(i, add_packet_to_queue[i], send_to_receiver[i])
	}

	for i := 0; i < n; i++ {
		go receiverSender(i, send_packet_from_queue[i], packet_from_queue[i], hosts_channels, send_to_receiver, return_actual_routing_table[i], all_hosts)
	}

	for i := 0; i < h; i++ {
		go host_func(all_hosts[i].r, all_hosts[i].h, send_to_receiver, hosts_channels[i], hosts_channels, all_hosts, i)
	}

	<-finished
	fmt.Println("")
	fmt.Println("Końcowy stan routing_table dla każdego wierzchołka")
	for i := 0; i < n; i++ {
		fmt.Println(r_table[i])
	}

}
