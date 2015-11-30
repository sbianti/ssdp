with Ada.Characters.Latin_1;
with Ada.Streams;
with Gnat.Sockets;

private package SSDP.Utils is
   use Ada.Characters, Ada.Streams, Gnat.Sockets;

   SSDP_Message_Min_Size: constant Natural := 16; -- obviously it should be more
   EOL: constant String := Latin_1.CR & Latin_1.LF;

   M_Search_Star_Line: constant String := "M-SEARCH * HTTP/1.1";
   Notify_Line: constant String := "NOTIFY * HTTP/1.1";
   Status_Line: constant String := "HTTP/1.1 200 OK";

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

   task Listener is
      entry Start(Job: in Job_Procedure_Access);
   end Listener;

   procedure Pl_Warning(Str: in String);

   procedure Pl_Debug(Str: in String);

   pragma Inline_Always(Pl_Warning, Pl_Debug);

   function To_String(Msg: in Stream_Element_Array) return String;

   function Create_Message(Static_Part: in String;
			   Headers: in Message_Header_Array) return String;

   procedure Send_Message(Device: in out Device_Type; Message: in String);

   procedure Activate_Multicast_Connection;

   procedure Start_Listening(Job: in Job_Procedure_Access);

   procedure Stop_Listening;

end SSDP.Utils;
