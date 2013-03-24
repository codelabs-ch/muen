Name:		ahven
Version:	1.8
Release:	1%{?dist}
Summary:	Unit Test Framework for Ada 95 Programming Language

Group:		Development/Libraries
License:	ISC
URL:		http://ahven.stronglytyped.org/
Source0:	http://downloads.sourceforge.net/%{name}/%{name}-%{version}.tar.gz
Patch0:		ahven-1.8-libdir.patch
BuildRoot:	%(mktemp -ud %{_tmppath}/%{name}-%{version}-%{release}-XXXXXX)

BuildRequires:	gcc-gnat
BuildRequires:	libgnat

%description
Ahven is a unit testing framework for Ada 95.

# Ahven has only static libahven.a library,
# no need for debug packages.
%global debug_package %{nil}

%prep
%setup -q
%patch0 -p1


%build
make %{?_smp_mflags}


%install
rm -rf $RPM_BUILD_ROOT
sed -e "s-../../lib/ahven-%{_libdir}/ahven-" -i gnat/ahven.gpr
make install PREFIX=$RPM_BUILD_ROOT/usr LIBDIR=$RPM_BUILD_ROOT%{_libdir}


%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%{_includedir}/ahven
%{_libdir}/ahven
%{_prefix}/lib/gnat

%changelog
* Thu Jul 22 2010 Tero Koskinen <tero.koskinen@iki.fi> - 1.8-1
- Updated to 1.8.
* Fri Feb 19 2010 Tero Koskinen <tero.koskinen@iki.fi> - 1.7-1
- Initial version.
