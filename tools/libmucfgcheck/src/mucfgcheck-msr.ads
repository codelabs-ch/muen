--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with Muxml;

package Mucfgcheck.MSR
is
   --D @Section Id => validation-msr, Label => Model Specific Registers (MSR), Parent => validation
   --D @Text Section => validation-msr
   --D The following checks are performed to verify Model Specific Register
   --D (MSR) specifications in the system policy.
   --D @UL Id => validators_msr, Section => validation-msr

   --D @Item List => validators_msr
   --D Validate that all MSR start addresses are smaller than end addresses.
   procedure Start_Smaller_End (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_msr
   --D Validate that subject MSRs are in the allowed list:
   --D \begin{itemize}
   --D \item \texttt{IA32\_SYSENTER\_CS/ESP/EIP}
   --D \item \texttt{IA32\_DEBUGCTL}
   --D \item \texttt{IA32\_EFER/STAR/LSTAR/CSTAR/FMASK}
   --D \item \texttt{IA32\_FS\_BASE/GS\_BASE/KERNEL\_GS\_BASE}
   --D \item \texttt{MSR\_PLATFORM\_INFO}
   --D \item \texttt{IA32\_THERM\_STATUS}
   --D \item \texttt{IA32\_TEMPERATURE\_TARGET}
   --D \item \texttt{IA32\_PACKAGE\_THERM\_STATUS}
   --D \end{itemize}
   procedure Check_Whitelist (XML_Data : Muxml.XML_Data_Type);

end Mucfgcheck.MSR;
