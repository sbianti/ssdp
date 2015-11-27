with Ada.Characters.Latin_1;
with Gnat.Sockets;

private package SSDP.Utils is
   use Ada.Characters;

   EOL: constant String := Latin_1.CR & Latin_1.LF;

   Multicast_Address: constant String := "239.255.255.250";
   Multicast_Port: constant Gnat.Sockets.Port_Type := 1900;

   function Create_Message(Static_Part: in String;
			   Headers: in Message_Header_Array) return String;

   procedure Send_Message(Message: in String);

end SSDP.Utils;
