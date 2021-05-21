with Ada.Text_IO; use Ada.Text_IO;
with Parameters;
with Ada.Integer_Text_IO;
with ada.numerics.discrete_random;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Numerics.Float_Random;

procedure routing_protocol is

    addition: array (0..Parameters.d-1, 0..1) of integer;
    i, j: integer := 0;

    package Integer_Vectors is new Ada.Containers.Vectors
    (Index_Type   => Natural,
    Element_Type => Integer);
    use Integer_Vectors;

    type Verticle is record
        Id : Integer;
        where_to_go: Vector;
    end record;

    type routing_table_item is record
        nexthop : Integer;
        cost : Integer;
        changed : Boolean;
    end record;


    edges: array (0..Parameters.n) of Vector;
    all_verticles: array (0..Parameters.n) of Verticle;
    type routing_table_type is array(0..Parameters.n-1) of routing_table_item;
    type routing_table_all_type is array(0..Parameters.n-1) of routing_table_type;
    r_table: routing_table_type;

    protected type Obj is
        procedure Set (new_table : routing_table_type);
        procedure Set2 (item : routing_table_item; id : Integer);
        procedure SetChange(id : Integer; new_changed : Boolean);
        function Get return routing_table_type;
    private
        table : routing_table_type;
    end Obj;

    protected body Obj is
        procedure Set (new_table : routing_table_type) is
        begin
            table := new_table;
        end Set;

        procedure Set2 (item : routing_table_item; id : Integer) is
        begin
            table(id) := item;
        end Set2;

        procedure SetChange(id : Integer; new_changed : Boolean) is
        begin
            table(id).changed := new_changed;
        end SetChange;

        function Get return routing_table_type is
        begin
            return table;
        end Get;
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
        vert: Vector := v.where_to_go;
        size: integer := 0;
    begin
        for i in 0..size loop
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

    function make_vector(i: in integer) return vector is
        V : Vector;
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
        V2 : Vector;
    begin
        for j in 0..Parameters.n loop
            V2 := make_vector(j);
            edges(j) := V2;
        end loop;
    end add_vectors;

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

    for i in 0..Parameters.n loop
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
        
    end loop;

    --for i in 0..Parameters.n-1 loop
        --for j in 0..Parameters.n-1 loop
            --Put(Integer'Image(routing_tables_all(i)(j).cost));
            --put(" ");
            --Put(Integer'Image(routing_tables_all(i)(j).nexthop));
            --put(" ");
            --put("   ");
        --end loop;
    --end loop;
    --new_line(1);

end routing_protocol;