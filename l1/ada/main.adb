with Ada.Text_IO; use Ada.Text_IO;
with Parameters;
with Ada.Integer_Text_IO;
with ada.numerics.discrete_random;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

procedure main is
   addition: array (0..Parameters.d-1, 0..1) of integer;
   i, j: integer := 0;

   package Integer_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Integer);
   use Integer_Vectors;

   -- package type of data(structure)
   type Packagee is record
        Id : Integer;
        visited_vertices : Vector;
   end record;

   -- verticle type of data(structure)
   type Verticle is record
      Id : Integer;
      packages: Vector;
      where_to_go: Vector;
      empty: Integer;
   end record;

   edges: array (0..Parameters.n-2) of Vector;
   all_packages: array(0..Parameters.k-1) of Packagee;
   all_verticles: array(0..Parameters.n-1) of Verticle;
   empty : Vector;
   v: Verticle;

   --Checking if exist
   function exist(x, y, size : in integer) return integer is
   i : integer := 0;
   begin
      for i in 0..size loop
         if addition(i,0) = x and addition(i,1) = y then
            return 1;
         end if;
      end loop;
      return 0;
   end exist;

   function exist2(x, y, size : in integer) return integer is
   i : integer := 0;
   begin
      if x+1 = y then
         return 1;
      end if;
      for i in 0..size loop
         if addition(i,0) = x and addition(i,1) = y then
            return 1;
         end if;
      end loop;
      return 0;
   end exist2;

   -- Random edges generation
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
         if num1 < num2 and exist(num1, num2, i-1) = 0  and num1 /= num2-1 then
            addition(i,0) := num1;
            addition(i,1) := num2;
            i := i + 1;
         end if;
      end loop;
   end randomN; 

   -- Creating vector for one vertex
   function make_vector(i: in integer) return vector is
   V : Vector;
   begin
      V.append(i+1);
      for j in 0..Parameters.d-1 loop
         if addition(j, 0) = i then
            V.append(addition(j, 1));
         end if;
      end loop;
      return V;
   end make_vector;

   -- Adding vectors to an array
   procedure add_vectors is
   V2 : Vector;
   begin
      for j in 0..Parameters.n-2 loop
         V2 := make_vector(j);
         edges(j) := V2;
      end loop;
   end add_vectors;

   -- thread of the printer
   task Printer is
      entry Go (Msg: String);
   end Printer ;

   task body Printer is
   begin
      loop
         accept Go (Msg : String) do
            put(Msg);
            delay 1.0;
         end Go;
      end loop;
   end Printer;



   -- thread of every vertex
   task type Taskvertex is
      entry Starttask (Vert: Verticle);
      entry Channel (Pack: Packagee);
   end Taskvertex;

   --array with tasks
   tasks: array(0..Parameters.n-1) of Taskvertex;

   task body Taskvertex is 
   ver: Verticle;
   numm, where : Integer;
   type randRange2 is new Integer range 1..Parameters.n-1;
   package Rand_Int is new ada.numerics.discrete_random(randRange2);
   use Rand_Int;
   gen2 : Generator;
   begin
      loop
         select
            accept Starttask (Vert: Verticle) do
               ver := Vert;
            end Starttask;
         or
            accept Channel (Pack: Packagee) do
               Printer.Go("pakiet " & Integer'Image(Pack.id) & " jest w wierzcholku " & Integer'Image(ver.id));
               numm := Integer(random(gen2));
               while exist2(ver.id, numm, Parameters.d-1) = 0 loop
                  numm := Integer(random(gen2));
               end loop;
               where := ver.where_to_go(numm);
               tasks(where).Channel(Pack);
            end Channel;
         end select;
      end loop;
   end Taskvertex;


   -- thread of the sender
   task Sender is
      entry StartSending;
   end Sender ;

   task body Sender is
   begin
         put("sender wysyla");
         accept StartSending do
            put("sender accept");
            for i2 in 0..Parameters.k-1 loop
               tasks(0).Channel(all_packages(i2));
               put("sender wyslal " & Integer'Image(all_packages(i2).id));
               delay 2.0;
            end loop;
         end StartSending; 
   end Sender;


   --thread of the receiver
   task Receiver is
      entry Receive (Pack: Packagee);
   end Receiver;

   task body Receiver is
   begin
      loop
         accept Receive (Pack: Packagee) do
            Printer.Go("pakiet " & Integer'Image(Pack.id) & " zostal odebrany");
            delay 2.0;
         end Receive;
      end loop;
   end Receiver;


begin
   randomN;
   add_vectors;
   for i in 0..Parameters.n-2 loop
      for j in 0..Integer(edges(i).Length)-1 loop
         Put(Integer'Image(i));
         put(" -> ");
         Put(Integer'Image(edges(i)(j)));
         put(" ");
         new_line(1);
      end loop;
   end loop;

   for i in 0..Parameters.k-1 loop
      all_packages(i).id := i;
      all_packages(i).visited_vertices := empty;
   end loop;

   for i in 0..Parameters.n-1 loop
      all_verticles(i).id := i;
      all_verticles(i).packages := empty;
      all_verticles(i).where_to_go := edges(i);
      all_verticles(i).empty := 1;
   end loop;

   Sender.StartSending;

   for i in 0..Parameters.n-1 loop
      v := all_verticles(i);
      tasks(i).Starttask(v);
   end loop;

   Printer.Go("Main");
   Receiver.Receive(all_packages(0));
end main;