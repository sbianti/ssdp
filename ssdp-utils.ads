with Ada.Characters.Latin_1;
with Ada.Streams;
with Gnat.Sockets;

private package SSDP.Utils is
   use Ada.Characters, Ada.Streams, Gnat.Sockets;

   EOL: constant String := Latin_1.CR & Latin_1.LF;

   Multicast_Address: constant String := "239.255.255.250";
   Multicast_Port: constant Gnat.Sockets.Port_Type := 1900;

   type Multicast_Connection is record
      Is_Listening: Boolean := False;
      Is_Down: Boolean := True;
      Socket: Socket_Type;
      Address: Sock_Addr_Type;
      Channel: Stream_Access;
   end record;

   Global_Multicast_Connection: Multicast_Connection;

   function To_String(Msg: in Stream_Element_Array) return String;

   function Create_Message(Static_Part: in String;
			   Headers: in Message_Header_Array) return String;

   procedure Send_Message(Device: in out Device_Type; Message: in String);

   procedure Activate_Multicast_Connection;

end SSDP.Utils;
