with Ada.Text_IO; use Ada.Text_IO;
with Parameters;
with Ada.Integer_Text_IO;
with ada.numerics.discrete_random;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Numerics.Float_Random;

procedure main is
   addition: array (0..Parameters.d-1, 0..1) of integer;
   addition2: array (0..Parameters.b-1, 0..1) of integer;
   i, j: integer := 0;


   package Integer_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Integer);
   use Integer_Vectors;

   -- package type of data(structure)
   type Packagee is record
      Id : Integer;
      Step : Integer;
      Dead : Integer;
      visited_vertices : Vector;
   end record;

   -- verticle type of data(structure)
   type Verticle is record
      Id : Integer;
      packages: Vector;
      where_to_go: Vector;
      empty: Integer;
   end record;

   edges: array (0..Parameters.n-1) of Vector;
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

   function exist3(x, y, size : in integer) return integer is
      i : integer := 0;
   begin
      for i in 0..size loop
         if addition2(i,0) = x and addition2(i,1) = y then
            return 1;
         end if;
      end loop;
      return 0;
   end exist3;

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
      i := 0;
      while i < Parameters.b loop
         num1 := Integer(random(gen));
         num2 := Integer(random(gen));
         if num1 > num2 and exist3(num1, num2, i-1) = 0 and num1 /= 0 and num1 /= Parameters.n-1 then
            addition2(i,0) := num1;
            addition2(i,1) := num2;
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
      for j in 0..Parameters.b-1 loop
         if addition2(j, 0) = i then
            V.append(addition2(j, 1));
         end if;
      end loop;
      return V;
   end make_vector;

   -- Adding vectors to an array
   procedure add_vectors is
      V2 : Vector;
   begin
      for j in 0..Parameters.n-1 loop
         V2 := make_vector(j);
         edges(j) := V2;
      end loop;
   end add_vectors;

   function get_delay return float is
      RNG : Ada.Numerics.Float_Random.Generator; 
      delay_s : Float;
   begin
      Ada.Numerics.Float_Random.reset(RNG);
      delay_s := Ada.Numerics.Float_Random.Random(RNG);
      return delay_s + Parameters.delayy;
   end get_delay;

   --thread of the printer
   task Printer is
      entry Go (Msg: String);
   end Printer ;

   task body Printer is
   d: float;
   begin
      loop
         select
            accept Go (Msg : String) do
               put_line(Msg);
               d := get_delay;
               delay duration(d);
            end Go;
         or
            terminate;
         end select;
      end loop;
   end Printer;


   -- thread of printing statistics
   task Statistics is
      entry Stat;
   end Statistics;

   task body Statistics is
   begin
      accept Stat do
         null;
      end Stat;

      put_line(" ");
      put_line("Obsluzone pakiety:");
      for i in 0..Parameters.n-1 loop
         put("Wierzcholek " & Integer'Image(i) & " : ");
         for j in 0..Integer(all_verticles(i).packages.Length)-1 loop
            put(Integer'Image(all_verticles(i).packages(j)));
            put(" ");
         end loop;
         put_line(" ");
      end loop;

      put_line(" ");
      put_line("Odwiedzone wierzcholki:");
      for i in 0..Parameters.k-1 loop
         put("Pakiet " & Integer'Image(i) & " : ");
         for j in 0..Integer(all_packages(i).visited_vertices.Length)-1 loop
            put(Integer'Image(all_packages(i).visited_vertices(j)));
            put(" ");
         end loop;
         put_line(" ");
      end loop;

   end Statistics;

   --thread of the receiver
   task Receiver is
      entry Start2;
      entry Receive (Pack: Packagee);
   end Receiver;

   task body Receiver is
   d: float;
   begin
       loop
         select
            accept Start2 do
               null;
            end Start2;
         or
            accept Receive (Pack: Packagee) do
               if Pack.Dead = 0 then
                  Printer.Go("pakiet " & Integer'Image(Pack.id) & " zostal odebrany");
               end if;
               d := get_delay;
               delay duration(d);
               if Pack.id = Parameters.k-1 then
                  d := get_delay;
                  delay duration(d);
                  Statistics.Stat;
                  end if;
            end Receive;
         or
            terminate;
         end select;
      end loop;
   end Receiver;


   -- thread of every vertex
   task type Taskvertex is
      entry Starttask (Vert: Verticle);
      entry Channel (Pack: Packagee);
      entry Traps;
   end Taskvertex;

   --array with tasks
   tasks: array(0..Parameters.n-1) of Taskvertex;

   task body Taskvertex is 
      ver: Verticle;
      numm, where : Integer;
      package Rand_Int is new ada.numerics.discrete_random(Integer);
      use Rand_Int;
      gen2 : Generator;
      tmppack : Packagee;
      d: float;
      trap: Integer;
   begin
      loop
         select
            accept Starttask (Vert: Verticle) do
               ver := Vert;
            end Starttask;
         or
            accept Traps do
               trap := 1;
            end Traps;
         or
            accept Channel (Pack: Packagee) do
               tmppack := Pack;
               if tmppack.Step <= Parameters.h  and trap = 0 then
                  all_verticles(ver.id).packages.append(Pack.id);
                  all_packages(Pack.id).visited_vertices.append(ver.id);
               end if;
            end Channel;

            if trap = 1 then
               Printer.Go("pakiet " & Integer'Image(tmppack.id) & " wpad?? w pu??apk?? ");
               trap := 0;
            else
               tmppack.Step := tmppack.Step + 1;
               if tmppack.Step > Parameters.h + 1 then
                  Printer.Go("pakiet " & Integer'Image(tmppack.id) & " umar?? ");
                  tmppack.Dead := 1;
                  Receiver.Receive(tmppack);
               else
                  if ver.id = Parameters.n-1  then
                  Printer.Go("pakiet " & Integer'Image(tmppack.id) & " jest w wierzcholku " & Integer'Image(ver.id));
                  Receiver.Receive(tmppack);
                  else
                     Printer.Go("pakiet " & Integer'Image(tmppack.id) & " jest w wierzcholku " & Integer'Image(ver.id));

                     if ver.where_to_go.Length = 1 then
                        numm := ver.id + 1;
                        where := ver.where_to_go(0);
                     else
                        numm := Integer(random(gen2)) mod Integer(ver.where_to_go.Length);
                        where := ver.where_to_go(numm);
                     end if;
                     tasks(where).Channel(tmppack);

                  end if;
               end if;
            end if;

            d := get_delay;
            delay duration(d);
         or
            terminate;

         end select;
      end loop;
   end Taskvertex;

   task Poacher is
      entry Start3;
   end Poacher;

   task body Poacher is
   package Rand_Int is new ada.numerics.discrete_random(Integer);
   use Rand_Int;
   gen3 : Generator;
   trap: integer;
   begin
       loop
         select
            accept Start3 do
               delay 0.8;
               trap := Integer(random(gen3)) mod Parameters.n-1;
               tasks(trap).Traps;
               delay 1.5;
            end Start3;
         or
            terminate;
            
         end select;
      end loop;
   end Poacher;

   -- thread of the sender
   task Sender is
      entry StartSending;
   end Sender ;

   task body Sender is
   d : float;
   begin
         accept StartSending do
            null;
         end StartSending; 
         for i2 in 0..Parameters.k-1 loop
               tasks(0).Channel(all_packages(i2));
               d := get_delay;
               delay duration(d);
         end loop;
   end Sender;

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
      all_packages(i).Step := 0;
      all_packages(i).Dead := 0;
      all_packages(i).visited_vertices := empty;
   end loop;

   for i in 0..Parameters.n-1 loop
      all_verticles(i).id := i;
      all_verticles(i).packages := empty;
      all_verticles(i).where_to_go := edges(i);
      all_verticles(i).empty := 1;
   end loop;

   Printer.Go(" ");

   for i in 0..Parameters.n-1 loop
      v := all_verticles(i);
      tasks(i).Starttask(v);
   end loop;

   Sender.StartSending;
   Receiver.Start2;
   Poacher.Start3;

end main;