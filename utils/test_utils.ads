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

with Ada.Text_IO;

package Test_Utils is
   -- Usefull subprograms for tests

   -- these are identical to their homonymes in private package SSDP.Utils:
   procedure Pl_Error(Str: in String);

   procedure Pl_Warning(Str: in String);

   procedure Pl_Debug(Str: in String);

end Test_Utils;
