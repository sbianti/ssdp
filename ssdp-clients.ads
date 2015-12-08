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

package SSDP.Clients is

   function To_US(Str: String) return Unbounded_String
     renames To_Unbounded_String;

   type SSDP_Client is new Device_Type with private;

   function Initialize(Service_Type, Universal_Serial_Number: in String)
		      return SSDP_Client;

   procedure Set_Universal_Serial_Number(Device: in out SSDP_Client;
					 Universal_Serial_Number: in String);

   procedure Set_Service_Type(Device: in out SSDP_Client;
			      Service_Type: in String);

   function Get_Universal_Serial_Number(Device: in SSDP_Client)
				       return String;

   function Get_Service_Type(Device: in SSDP_Client) return String;

   procedure M_Search(Device: in SSDP_Client;
		      Other_Header: in Message_Header_Array);

   procedure Start_Listening;

   procedure Stop_Listening;

private

   type SSDP_Client is new Device_Type with null record;

end SSDP.Clients;
