--------------------------------------------------------------------------------
--  This file is part of SSDP package   				      --
--  									      --
--  Copyright © 2015 Sébastien Bianti					      --
--  									      --
--  This program is free software; you can redistribute it and/or modify      --
--  it under the terms of the GNU General Public License version 3 as	      --
--  published by the Free Software Foundation.				      --
--  									      --
--  This program is distributed in the hope that it will be useful,	      --
--  but WITHOUT ANY WARRANTY; without even the implied warranty of	      --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the	      --
--  GNU General Public License for more details.			      --
--  									      --
--  You should have received a copy of the GNU General Public License	      --
--  along with this program; if not, write to the Free Software	              --
--  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA --
--------------------------------------------------------------------------------

with Ada.Characters.Latin_1;
with Ada.Streams;

private package SSDP.Utils is
   use Ada.Characters, Ada.Streams, Gnat.Sockets;

   SSDP_Message_Min_Size: constant Natural := 16; -- obviously it should be more
   EOL: constant String := Latin_1.CR & Latin_1.LF;

   M_Search_Star_Line: constant String := "M-SEARCH * HTTP/1.1";
   Notify_Line: constant String := "NOTIFY * HTTP/1.1";
   Status_Line: constant String := "HTTP/1.1 200 OK";

   type Connection_Type is (Multicast, Unicast);
   type Socket_Array is array (Connection_Type) of Socket_Type;

   type Network_Settings is record
      Is_Listening: Boolean := False;
      Is_Down: Boolean := True;
      Socket: Socket_Array;
      Local_Address: Sock_Addr_Type;
      Channel: Stream_Access; -- only for multicast emission
   end record;

   Global_Network_Settings: Network_Settings;

   -- Useful type for message parsing in job_procedure:
   type Line_Array is array (Natural range <>) of access String;

   task Listener is
      entry Start(Job: in Job_Procedure_Access);
   end Listener;

   procedure Pl_Error(Str: in String);

   procedure Pl_Warning(Str: in String);

   procedure Pl_Debug(Str: in String);

   pragma Inline_Always(Pl_Error, Pl_Warning, Pl_Debug);

   function To_String(Msg: in Stream_Element_Array) return String;

   function Create_Message(Static_Part: in String;
			   Headers: in Message_Header_Array) return String;

   procedure Send_Message(Message: in String);

   procedure Send_Message(Message: in String; Address: in Sock_Addr_Type);

   procedure Set_Networking;

   function Parse_Lines(Message: in String) return Line_Array;

   procedure Start_Listening(Job: in Job_Procedure_Access);

   procedure Stop_Listening;

end SSDP.Utils;
