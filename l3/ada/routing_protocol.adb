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

    package Integer_Vectors is new Ada.Containers.Vectors(Index_Type   => Natural, Element_Type => Integer);
    type package_item is record
        j : Integer;
        cost: Integer;
    end record;

    package Package_Vectors is new Ada.Containers.Vectors(Index_Type   => Natural, Element_Type => package_item);

    type Verticle is record
        Id : Integer;
        where_to_go: Integer_Vectors.Vector;
    end record;

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
            put_line("routing_table " & Integer'Image(my_id) & " podmienia na pozycji " & Integer'Image(id) & " na { " & Integer'Image(item.nexthop) & " " & Integer'Image(item.cost) & " }");
            table(id) := item;
        end Set2;

        procedure SetChange(id : Integer; new_changed : Boolean) is
        begin
            put_line("routing_table" & Integer'Image(my_id) & " zmienia changed na pozycji " & Integer'Image(id));
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
   msg : unbounded_string;
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
               msg := To_Unbounded_String("");
               append(msg, "receiver " & Integer'Image(id_receiver) & " otrzymal pakiet [");
                size := Standard.Integer(p.Length);
                for i in 0..size-1 loop
                    actual_packet := p(i);
                    append(msg, "{" & Integer'Image(actual_packet.j) & " " & Integer'Image(actual_packet.cost) & "} ");
                    actual_routing_table := protected_routing_tables(id_receiver).Get;
                    new_cost := 1 + actual_packet.cost;
                    if new_cost < actual_routing_table(actual_packet.j).cost then
                        new_routing_table_item.cost := new_cost;
                        new_routing_table_item.nexthop := l;
                        new_routing_table_item.changed := TRUE;
                        protected_routing_tables(id_receiver).Set2(new_routing_table_item, actual_packet.j);
                    end if;
                end loop;
                append(msg, "]");
                Printer.Go(to_string(msg));
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
   msg : unbounded_string;
   begin
            accept Start(V : Verticle) do
               verticlee := V;
               id := verticlee.id;
            end Start;
            loop
                delay 0.5;
                package_to_send.Clear;
                msg := To_Unbounded_String("");
                append(msg, "sender " & Integer'Image(id) & " wysyla pakiet [");
                routing_tablee := protected_routing_tables(id).Get;
                for i in 0..Parameters.n-1 loop
                    if routing_tablee(i).changed = TRUE then
                        protected_routing_tables(id).SetChange(i, FALSE);
                        package_itemm.j := i;
                        package_itemm.cost := routing_tablee(i).cost;
                        package_to_send.append(package_itemm);
                        append(msg, "{" & Integer'Image(package_itemm.j) & " " & Integer'Image(package_itemm.cost) & "} ");
                    end if;
                end loop;
                append(msg, "]");
                to_send_pck.l := id;
                to_send_pck.packett := package_to_send;
                    
                if package_to_send.Length /= 0 then
                    size := Standard.Integer(verticlee.where_to_go.Length);
                    Printer.Go(to_string(msg));
                    for i in 0..size-1 loop
                        tasks(verticlee.where_to_go(i)).GetPacket(to_send_pck);
                    end loop;
                end if;
            end loop;
   end Sender;


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

    for i in 0..Parameters.n-1 loop
        all_verticles(i).id := i;
        all_verticles(i).where_to_go := edges(i);
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

    delay 40.0;
    put_line("");
    put_line("Koncowy stan routing_tables");
    for i in 0..Parameters.n-1 loop
        protected_routing_tables(i).PrintStatistics;
    end loop;


end routing_protocol;