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

package SSDP.Service_Provider is

   function To_US(Str: String) return Unbounded_String
     renames To_Unbounded_String;

   type Service_Provider_Device_Type is new Device_Type with private;

   function Initialize_Device(Service_Type, Universal_Serial_Number,
				Location, AL, -- only one is required
				Cache_Control, Expires: String) -- dito
			     return Service_Provider_Device_Type;

   procedure M_Search_Response(Device: in Service_Provider_Device_Type;
			       USN_Requester: in String;
			       Other_Headers: in Message_Header_Array);

   procedure Notify_Alive(Device: in Service_Provider_Device_Type;
			  Other_Headers: in Message_Header_Array);

   procedure Notify_Bye_Bye(Device: in Service_Provider_Device_Type);

   procedure Start_Listening;

   procedure Stop_Listening;

   Bad_Service: exception;
private
   type Service_Provider_Device_Type is new Device_Type with record
      Location, AL, -- only one is required
      Cache_Control, Expires: Unbounded_String; -- dito
   end record;
end SSDP.Service_Provider;
