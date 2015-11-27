package body SSDP.Utils is
   use Gnat.Sockets;

   function Create_Message(Static_Part: in String;
			   Headers: in Message_Header_Array) return String is
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

   procedure Send_Message(Message: in String) is
      Socket: Socket_Type;
      Address: Sock_Addr_Type;
      Channel: Stream_Access;
   begin
      Address.Addr := Inet_Addr(Multicast_Address);
      Address.Port := Multicast_Port;
      Create_Socket(Socket, Family_Inet, Socket_Datagram);
      Set_Socket_Option(Socket, Ip_Protocol_For_Ip_Level,
			(Add_Membership, Address.Addr, Any_Inet_Addr));
      Channel := Stream(Socket, Address);
      String'Output(Channel, Message);
   end Send_Message;

end SSDP.Utils;
