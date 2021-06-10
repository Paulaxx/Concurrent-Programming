with Ada.Text_IO; use Ada.Text_IO;
with Parameters;
with Ada.Integer_Text_IO;
with ada.numerics.discrete_random;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Numerics.Float_Random;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;


procedure routing_protocol is

    addition: array (0..Parameters.d-1, 0..1) of integer;
    i, j: integer := 0;
    counter: Integer;

    package Integer_Vectors is new Ada.Containers.Vectors(Index_Type   => Natural, Element_Type => Integer);
    type package_item is record
        j : Integer;
        cost: Integer;
    end record;

    package Package_Vectors is new Ada.Containers.Vectors(Index_Type   => Natural, Element_Type => package_item);

    type Host is record
        r : Integer;
        h : Integer;
    end record;

    in_which_position_first_host : Integer_Vectors.Vector;

    package Hosts_Vectors is new Ada.Containers.Vectors(Index_Type => Natural, Element_Type => Host);
    all_hosts: Hosts_Vectors.Vector;

    type Package_standard is record
        receiver : Host;
        sender : Host;
        visited : Integer_Vectors.Vector;
    end record;
    package Package_Standard_Vectors is new Ada.Containers.Vectors(Index_Type   => Natural, Element_Type => Package_standard);


    protected type ForwarderQueue is
    procedure Add(item : Package_standard);
    function Get return Package_Standard_Vectors.Vector;
    procedure Swap (new_queue : Package_Standard_Vectors.Vector);
    private
        queue : Package_Standard_Vectors.Vector;
    end ForwarderQueue;

    protected body ForwarderQueue is
        procedure Add (item : Package_standard) is
        begin
            queue.append(item);
        end Add;

        procedure Swap (new_queue : Package_Standard_Vectors.Vector) is
        begin
            queue := new_queue;
        end Swap;

        function Get return Package_Standard_Vectors.Vector is
        begin
            return queue;
        end Get;

    end ForwarderQueue;

    protected_queues: array (0..Parameters.n-1) of ForwarderQueue;

    task type Host_task is
    entry Start(H : Host; id: Integer);
    entry SendPacket(packet : Package_standard);
    end Host_task;

    host_tasks: array(0..Parameters.n*Parameters.n) of Host_task;
    host_tasks_capacity: Integer;

    type Verticle is record
        Id : Integer;
        where_to_go: Integer_Vectors.Vector;
        hosts : Hosts_Vectors.Vector;
        how_many_hosts : Integer;
    end record;

    task type ForwarderReceiver is
        entry StartForwarderReceiver(V : Verticle);
        entry GetStandardPacket(packet : Package_standard);
    end ForwarderReceiver;

    ForwarderReceivers: array(0..Parameters.n-1) of ForwarderReceiver;

    task body ForwarderReceiver is
    verticlee : Verticle;
    id: integer;
    begin
            loop
                select
                    accept StartForwarderReceiver(V : Verticle) do
                        verticlee := V;
                        id := verticlee.id;
                    end StartForwarderReceiver;
                or
                    accept GetStandardPacket(packet : Package_standard) do
                        protected_queues(id).Add(packet);
                    end GetStandardPacket;
                end select;
            end loop;
    end ForwarderReceiver;

    task body Host_task is
        hostt : Host;
        package Rand_Int3 is new ada.numerics.discrete_random(Integer);
        use Rand_Int3;
        gen3 : Generator;
        random_host : Integer;
        pack : Package_standard;
        empty_vector : Integer_Vectors.Vector;
        verticle_id : Integer;
        position : Integer;
        size : Integer;
        packet2 : Package_standard;
        host_id : Integer;
    begin
        accept Start(H : Host; id: Integer) do
            reset(gen3);
            hostt := H;
            host_id := id;
        end Start;
        verticle_id := hostt.r;
        random_host := Integer(random(gen3)) mod Integer(counter);
        while random_host = host_id loop
            reset(gen3);
            random_host := Integer(random(gen3)) mod Integer(counter);
        end loop;
        pack.receiver := all_hosts(random_host);
        pack.sender := hostt;
        pack.visited := empty_vector;
        ForwarderReceivers(hostt.r).GetStandardPacket(pack);
        loop
            accept SendPacket(packet : Package_standard) do
                packet2 := packet;
            end SendPacket;
            put("packet from host h:" & Integer'Image(packet2.sender.h) & " r:" & Integer'Image(packet2.sender.r) & " to host h:" & Integer'Image(packet2.receiver.h) 
            & " r:" & Integer'Image(packet2.receiver.r) & " with visited routers: ");
            size := Integer(packet2.visited.Length);
            for i in 0..size-1 loop
                put(Integer'Image(packet2.visited(i)) & " ");
            end loop;
            put_line("");
            delay 0.5;
            packet2.receiver := packet2.sender;
            packet2.sender := hostt;
            packet2.visited := empty_vector;
            position := in_which_position_first_host(verticle_id) + hostt.h;
            ForwarderReceivers(hostt.r).GetStandardPacket(packet2);
        end loop;
    end Host_task;


    produced_hosts : Hosts_Vectors.Vector;
    new_host : Host_task;

    type routing_table_item is record
        nexthop : Integer;
        cost : Integer;
        changed : Boolean;
    end record;

    type to_send is record
        l : Integer;
        packett : Package_Vectors.Vector;
    end record;

    edges: array (0..Parameters.n-1) of Integer_Vectors.Vector;
    all_verticles: array (0..Parameters.n-1) of Verticle;
    type routing_table_type is array(0..Parameters.n-1) of routing_table_item;
    type routing_table_all_type is array(0..Parameters.n-1) of routing_table_type;
    r_table: routing_table_type;

    task Printer is
        entry Go (Msg: String);
    end Printer ;

    task body Printer is
    begin
        loop
            select
                accept Go (Msg : String) do
                    put(Msg);
                    put_line("");
                    delay 1.0;
                end Go;
            or
                terminate;
            end select;
        end loop;
    end Printer;

    protected type Obj is
        procedure Set (new_table : routing_table_type);
        procedure Set2 (item : routing_table_item; id : Integer);
        procedure SetId (idd : Integer);
        procedure SetChange(id : Integer; new_changed : Boolean);
        function Get return routing_table_type;
        procedure PrintStatistics;
    private
        table : routing_table_type;
        my_id : Integer;
    end Obj;

    protected body Obj is
        procedure Set (new_table : routing_table_type) is
        begin
            table := new_table;
        end Set;

        procedure SetId (idd : Integer) is
        begin
            my_id := idd;
        end SetId;

        procedure Set2 (item : routing_table_item; id : Integer) is
        begin
            table(id) := item;
            put_line("routing table " & Integer'Image(my_id) & " changed at position " & Integer'Image(id) & " to routing_table_item: [nexthop: " & Integer'Image(item.nexthop) & " cost: "
            & Integer'Image(item.cost) & "]");
        end Set2;

        procedure SetChange(id : Integer; new_changed : Boolean) is
        begin
            table(id).changed := new_changed;
        end SetChange;

        function Get return routing_table_type is
        begin
            return table;
        end Get;

        procedure PrintStatistics is
        begin
            put("[");
            for i in 0..Parameters.n-1 loop
                put("{" & Integer'Image(table(i).nexthop) & " " & Integer'Image(table(i).cost) & "} " );
            end loop;
            put("]");
            put_line("");
        end PrintStatistics;
    end Obj;

    protected_routing_tables: array (0..Parameters.n-1) of Obj;

    function exist(x, y, size : in integer) return integer is
        i : integer := 0;
    begin
        for i in 0..size loop
            if (addition(i,0) = x and addition(i,1) = y) or (addition(i,0) = y and addition(i,1) = x) then
                return 1;
            end if;
        end loop;
        return 0;
    end exist;

    function neighbor(v : Verticle; n : integer) return integer is
        i : integer := 0;
        vert: Integer_Vectors.Vector := v.where_to_go;
        size: integer := Integer(v.where_to_go.Length);
    begin
        for i in 0..size-1 loop
            if vert(i) = n then
                return 1;
            end if;
        end loop;
        return 0;
    end neighbor;

    function produce_hosts(Id : Integer) return Hosts_Vectors.Vector is
    new_hosts: Hosts_Vectors.Vector;
    type randRange2 is new Integer range 0..(1);
    package Rand_Int2 is new ada.numerics.discrete_random(randRange2);
    use Rand_Int2;
    gen2 : Generator;
    hosts_num : integer;
    i : integer := 0;
    host_item : Host;
    begin
        reset(gen2);
        hosts_num := Integer(random(gen2));
        for i in 0..hosts_num-1 loop
            host_item.r := Id;
            host_item.h := i;
            new_hosts.append(host_item);
            all_hosts.append(host_item);
        end loop;
        return new_hosts;
    end produce_hosts;

    procedure randomN is
        type randRange is new Integer range 0..Parameters.n-1;
        package Rand_Int is new ada.numerics.discrete_random(randRange);
        use Rand_Int;
        gen : Generator;
        num1 : integer;
        num2 : integer;
        i : integer := 0;
    begin
        reset(gen);
        while i < Parameters.d loop
            num1 := Integer(random(gen));
            num2 := Integer(random(gen));
            if num1 /= num2 and exist(num1, num2, i-1) = 0  and num1 /= num2-1 and num2 /= num1-1 then
                addition(i,0) := num1;
                addition(i,1) := num2;
                i := i + 1;
            end if;
        end loop;
    end randomN; 

    function make_vector(i: in integer) return Integer_Vectors.vector is
        V : Integer_Vectors.Vector;
    begin
        if i < Parameters.n-1 then
            V.append(i+1);
        end if;
        if i > 0 then
            V.append(i-1);
        end if;
        for j in 0..Parameters.d-1 loop
            if addition(j, 0) = i then
                V.append(addition(j, 1));
            end if;
            if addition(j, 1) = i then
                V.append(addition(j, 0));
            end if;
        end loop;
        return V;
    end make_vector;

    procedure add_vectors is
        V2 : Integer_Vectors.Vector;
    begin
        for j in 0..Parameters.n-1 loop
            V2 := make_vector(j);
            edges(j) := V2;
        end loop;
    end add_vectors;

    task type Receiver is
        entry Start2(Idd : integer);
        entry GetPacket(packet : to_send);
    end Receiver;

    tasks: array(0..Parameters.n-1) of Receiver;

    task body Receiver is
        id_receiver: integer;
        new_routing_table_item : routing_table_item;
        actual_packet : package_item;
        actual_routing_table : routing_table_type;
        new_cost : Integer;
        p : Package_Vectors.Vector;
        l : Integer;
        size : integer;
    begin
        loop
            select
            accept Start2(Idd : integer) do
                id_receiver := Idd;
            end Start2;
        or
            accept GetPacket(packet : to_send) do
                p := packet.packett;
                l := packet.l;
                size := Standard.Integer(p.Length);
                for i in 0..size-1 loop
                    actual_packet := p(i);
                    actual_routing_table := protected_routing_tables(id_receiver).Get;
                    new_cost := 1 + actual_packet.cost;
                    if new_cost < actual_routing_table(actual_packet.j).cost then
                        new_routing_table_item.cost := new_cost;
                        new_routing_table_item.nexthop := l;
                        new_routing_table_item.changed := TRUE;
                        protected_routing_tables(id_receiver).Set2(new_routing_table_item, actual_packet.j);
                    end if;
                end loop;
            end GetPacket;
            end select;
        end loop;
    end Receiver;


    task type Sender is
        entry Start(V : Verticle);
    end Sender;

    tasks2: array(0..Parameters.n-1) of Sender;

    task body Sender is
    verticlee : Verticle;
    id: integer;
    routing_tablee : routing_table_type;
    package_itemm : package_item;
    to_send_pck : to_send;
    package_to_send : Package_Vectors.Vector;
    size : Integer;
    begin
            accept Start(V : Verticle) do
                verticlee := V;
                id := verticlee.id;
            end Start;
            delay 6.0;
            loop
                delay 1.0;
                package_to_send.Clear;
                routing_tablee := protected_routing_tables(id).Get;
                for i in 0..Parameters.n-1 loop
                    if routing_tablee(i).changed = TRUE then
                        protected_routing_tables(id).SetChange(i, FALSE);
                        package_itemm.j := i;
                        package_itemm.cost := routing_tablee(i).cost;
                        package_to_send.append(package_itemm);
                    end if;
                end loop;
                to_send_pck.l := id;
                to_send_pck.packett := package_to_send;
                    
                if package_to_send.Length /= 0 then
                    size := Standard.Integer(verticlee.where_to_go.Length);
                    for i in 0..size-1 loop
                        tasks(verticlee.where_to_go(i)).GetPacket(to_send_pck);
                    end loop;
                end if;
            end loop;
    end Sender;

    task type ForwarderSender is
        entry StartForwarderSender(V : Verticle);
    end ForwarderSender;

    ForwarderSenders: array(0..Parameters.n-1) of ForwarderSender;

    task body ForwarderSender is
    verticlee : Verticle;
    id: integer;
    sended_pack : Package_standard;
    routing_tablee : routing_table_type;
    n : Integer;
    queue: Package_Standard_Vectors.Vector;
    position : Integer;
    begin
            accept StartForwarderSender(V : Verticle) do
                verticlee := V;
                id := verticlee.id;
            end StartForwarderSender;
            loop
                delay 0.5;
                queue := protected_queues(id).Get;
                if queue.Length > 0 then
                    sended_pack := queue(0);
                    Package_Standard_Vectors.Delete_First(queue, 1);
                    protected_queues(id).Swap(queue);
                    sended_pack.visited.append(id);
                    if sended_pack.receiver.r = id then
                        position := in_which_position_first_host(id) + sended_pack.receiver.h;
                        host_tasks(position).SendPacket(sended_pack);
                    else
                        routing_tablee := protected_routing_tables(id).Get;
                        n := routing_tablee(sended_pack.receiver.r).nexthop;
                        ForwarderReceivers(n).GetStandardPacket(sended_pack);
                    end if;
                end if;
            end loop;
    end ForwarderSender;

    size: Integer;
    size2: Integer;

begin
    randomN;
    add_vectors;
    for i in 0..Parameters.n-1 loop
        for j in 0..Integer(edges(i).Length)-1 loop
            Put(Integer'Image(i));
            put(" - ");
            Put(Integer'Image(edges(i)(j)));
            put(" ");
            new_line(1);
        end loop;
    end loop;

    counter := 0;
    for i in 0..Parameters.n-1 loop
        all_verticles(i).id := i;
        all_verticles(i).where_to_go := edges(i);
        produced_hosts := produce_hosts(i);
        Put(Integer'Image(i));
        size2 := Integer(produced_hosts.Length);
        put(": ");
        for j in 0..size2-1 loop
            put(Integer'Image(produced_hosts(j).r));
            put(" ");
            put(Integer'Image(produced_hosts(j).h));
            put(", ");
        end loop;
        put_line("");
        all_verticles(i).hosts := produced_hosts;
        all_verticles(i).how_many_hosts := Integer(produced_hosts.Length);
        in_which_position_first_host.append(counter);
        counter := counter + Integer(produced_hosts.Length);
    end loop;

    for i in 0..Parameters.n-1 loop
        for j in 0..Parameters.n-1 loop
            if neighbor(all_verticles(i), j) = 1 then
                r_table(j).cost := 1;
                r_table(j).nexthop := j;
                r_table(j).changed := TRUE;
            elsif i < j then
                r_table(j).cost := j-i;
                r_table(j).nexthop := i+1;
                r_table(j).changed := TRUE;
            elsif i > j then
                r_table(j).cost := i-j;
                r_table(j).nexthop := i-1;
                r_table(j).changed := TRUE;
            else
                r_table(j).cost := 0;
                r_table(j).nexthop := 0;
                r_table(j).changed := FALSE;
            end if;
        end loop;
        protected_routing_tables(i).Set(r_table);
        protected_routing_tables(i).SetId(i);
        
    end loop;

    for i in 0..Parameters.n-1 loop
        tasks(i).Start2(i);
    end loop;
    for i in 0..Parameters.n-1 loop
        tasks2(i).Start(all_verticles(i));
    end loop;
    for i in 0..Parameters.n-1 loop
        ForwarderReceivers(i).StartForwarderReceiver(all_verticles(i));
    end loop;
    for i in 0..Parameters.n-1 loop
        ForwarderSenders(i).StartForwarderSender(all_verticles(i));
    end loop;

    host_tasks_capacity := 0;
    for i in 0..Parameters.n-1 loop
        produced_hosts := all_verticles(i).hosts;
        size := Integer(produced_hosts.Length);
        for j in 0..size-1 loop
            host_tasks(host_tasks_capacity).Start(produced_hosts(j), host_tasks_capacity);
            host_tasks_capacity := host_tasks_capacity + 1;
        end loop;
    end loop;

    delay 40.0;
    put_line("");
    put_line("Koncowy stan routing_tables");
    for i in 0..Parameters.n-1 loop
        protected_routing_tables(i).PrintStatistics;
    end loop;

-- packet from host h: 2 r: 5 to host h: 0 r: 1 with visited routers:  5  6  2  1 
-- packet from host h: 0 r: 2 to host h: 3 r: 2 with visited routers:  2 
-- packet from host h: 1 r: 8 to host h: 1 r: 3 with visited routers:  8  7  1  2  3 

end routing_protocol;