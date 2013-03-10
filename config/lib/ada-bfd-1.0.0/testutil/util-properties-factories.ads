-----------------------------------------------------------------------
--  factories -- Factory for property manager implementation
--  Copyright (C) 2001, 2002, 2003, 2006, 2008, 2009, 2010 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------
private package Util.Properties.Factories is

   --  Initialize the property manager by setting the implementation.
   procedure Initialize (Properties : in out Manager);

   --  Set the default implementation factory for property manager.
   procedure Set_Default_Factory (Factory : Interface_P.Manager_Factory);

end Util.Properties.Factories;
