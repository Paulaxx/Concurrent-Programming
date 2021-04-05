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

   edges: array (0..Parameters.n-2) of Vector;

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
end main;