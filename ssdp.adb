with Ada.Characters.Latin_1;
with Gnat.Sockets;

package body SSDP is
   use Ada.Characters.Latin_1, Gnat.Sockets;

   EOL: constant String := CR & LF;
   Multicast_Address: constant String := "239.255.255.250";
   Multicast_Port: constant Port_Type := 1900;

   function Create_Message(Static_Part: in String;
			   Headers: in Message_Header_Array)
			  return String is
      Size: Natural := Static_Part'Length + Headers'Length * EOL'Length +
	EOL'Length; -- the final EOL
   begin
      for N in Headers'Range loop
	 Size := Size + Length(Headers(N));
      end loop;

      declare
	 Message: String(1..Size);
	 Index: Positive := Static_Part'Length;
	 Last: Natural;
      begin
	 Message(1..Index) := Static_Part;
	 Index := Index + 1;

	 for N in Headers'Range loop
	    Last := Index + Length(Headers(N)) + EOL'Length - 1;

	    Message(Index..Last) := To_String(Headers(N)) & EOL;
	    Index := Last + 1;
	 end loop;

	 Message(Index..Message'Last) := EOL;

	 return Message;
      end;
   end Create_Message;

   function M_Search(Service_Type: in String;
		     Universal_Serial_Number: in String;
		     Other_Header: in Message_Header_Array) return String is
      Start_Line: constant String := "M-SEARCH * HTTP/1.1" & EOL;
   begin
      if Service_Type = "" then raise Header_Malformed
	with "Header «ST» (Service Type) is missing";
      end if;

      if Universal_Serial_Number = "" then raise Header_Malformed
	with "Header «S» (Universal Service Type) is missing";
      end if;

      return Create_Message(Start_Line & "ST: " & Service_Type & EOL &
			      "USN: " & Universal_Serial_Number & EOL,
			    Other_Header);
   end M_Search;

   function M_Search_Response(Service_Type: in String;
			      Universal_Serial_Number: in String;
			      USN_Requester: in String;
			      Other_Headers: in Message_Header_Array)
			     return String is
      Start_Line: constant String := "HTTP/1.1 200 OK" & EOL;
   begin
      if Service_Type = "" then raise Header_Malformed
	with "Header «ST» (Service Type) is missing";
      end if;

      if Universal_Serial_Number = "" then raise Header_Malformed
	with "Header «USN» (Universal Service Type) is missing";
      end if;

      if USN_Requester = "" then raise Header_Malformed
	with "Header «S» (Universal Service Type of the requester) is missing";
      end if;

      return "";
   end M_Search_Response;

   function Notify_Alive(Service_Type,
			 Universal_Serial_Number,
			 Location, AL, -- only one is required
			 Cache_Control, Expires: in String; -- dito
			 Other_Headers: in Message_Header_Array)
			return String is
      Start_Line: Unbounded_String;
   begin
      if Service_Type = "" then raise Header_Malformed
	with "Header «NT» (Service Type) is missing";
      end if;

      if Universal_Serial_Number = "" then raise Header_Malformed
	with "Header «USN» (Universal Service Type) is missing";
      end if;

      if Location = "" and AL = "" then raise Header_Malformed
	with "Both headers «Location» and «AL» are missing," &
	" at least one is required";
      end if;

      if Cache_Control = "" and Expires = "" then raise Header_Malformed
	with "Both headers «Cache-Control» and «Expires» are missing," &
	" at least one is required";
      end if;

      Start_Line := To_US("NOTIFY * HTTP/1.1" & EOL &
			    "NT: " & Service_Type & EOL &
			    "USN: " & Universal_Serial_Number & EOL &
			    "NTS: ssdp:alive" & EOL);

      if Location /= "" then
	 Append(Start_Line, To_US("Location: " & Location & EOL));
      end if;

      if AL /= "" then
	 Append(Start_Line, To_US("AL: " & AL & EOL));
      end if;

      if Cache_Control /= "" then
	 Append(Start_Line, To_US("Cache-Control: " & Cache_Control & EOL));
      end if;

      if Expires /= "" then
	 Start_Line := Start_Line & To_US("Expires: " & Expires & EOL);
      end if;

      return Create_Message(To_String(Start_Line), Other_Headers);
   end Notify_Alive;

   function Notify_Byebye(Service_Type: in String;
			  Universal_Serial_Number: in String)  return String is
      Start_Line: constant String := "NOTIFY * HTTP/1.1" & EOL;
   begin
      if Service_Type = "" then raise Header_Malformed
	with "Header «NT» (Service Type) is missing";
      end if;

      if Universal_Serial_Number = "" then raise Header_Malformed
	with "Header «USN» (Universal Service Type) is missing";
      end if;

      return Start_Line & "NT: " & Service_Type & EOL &
	"USN: " & Universal_Serial_Number & EOL &
	"NTS: ssdp:byebye" & EOL & EOL;
   end Notify_Byebye;

   procedure Send_Message(Message: in String) is
      Socket: Socket_Type;
      Address: Sock_Addr_Type;
      Channel: Stream_Access;
   begin
      Address.Addr := Inet_Addr(Multicast_Address);
      Address.Port := Multicast_Port;
      Create_Socket(Socket, Family_Inet, Socket_Datagram);
      Set_Socket_Option(Socket,	Ip_Protocol_For_Ip_Level,
			(Add_Membership, Address.Addr, Any_Inet_Addr));
      Channel := Stream(Socket, Address);
      String'Output(Channel, Message);
   end Send_Message;
end SSDP;
