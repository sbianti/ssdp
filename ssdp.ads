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

with Ada.Strings.Unbounded;

package SSDP is
   use Ada.Strings.Unbounded;

   type US_Array is array (Positive range <>) of Unbounded_String;
   subtype Location_Array is US_Array;
   subtype Message_Header_Array is US_Array;

   type Device_Type is tagged private;

   type Job_Procedure_Access is access procedure;

   ----------------------------------------------------------------------------
   --  SSDP messages are http 1.1 messages where:			     --
   --   ⋅service type URI represents the type of service (ex: refrigerator)  --
   --   ⋅USN is a URI which uniquely identifies a particular instance of a   --
   --  service, ex: upnp:uuid:bla:… for a device that has a UUID             --
   ----------------------------------------------------------------------------

   type Command_Name_Type is (Discover, Sleep, Alive, Bye_Bye);

   -- Restricted command_name types:
   type Client_Command_Name_Type is new Command_Name_Type range Discover..Sleep;
   type Service_Command_Name_Type is new Command_Name_Type range Sleep..Bye_Bye;

   Header_Malformed,
   Not_An_SSDP_Message,
   SSDP_Message_Malformed: exception;

private

   type Device_Type is tagged record
      Service_Type,
      Universal_Serial_Number: Unbounded_String;
   end record;

end SSDP;
