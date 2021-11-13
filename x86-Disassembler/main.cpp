#include "magic_enum.hpp"
#include "disassembly.hpp"

#include <iostream>
#include <fstream>
#include <Windows.h>

int main()
{
	/*
		Get the executable section of the PE
	*/
	std::fstream file(
		"C:\\Users\\GabeLaptop\\Documents\\GitHub\\GabeMillikan\\SimpleExe\\Release\\SimpleExe.exe",
		std::ios::in | std::ios::binary
	);

	file.seekg(0, std::ios::end);
	size_t fileSz = (size_t)file.tellg();
	file.seekg(0);

	char* dll = new char[fileSz];
	file.read(dll, fileSz);

	const IMAGE_DOS_HEADER& dosHeader = *(IMAGE_DOS_HEADER*)dll;
	const IMAGE_NT_HEADERS& ntHeaders = *(IMAGE_NT_HEADERS*)(dll + dosHeader.e_lfanew);
	IMAGE_SECTION_HEADER* sectionHeaders = (IMAGE_SECTION_HEADER*)(dll + dosHeader.e_lfanew + sizeof(IMAGE_NT_HEADERS));

	int executableSection = -1;
	for (size_t i = 0; i < ntHeaders.FileHeader.NumberOfSections; i++)
	{
		const auto& section = sectionHeaders[i];

		if (!strcmp(".text", (char*)section.Name))
		{
			executableSection = i;
		}

		if (
			section.VirtualAddress <= ntHeaders.OptionalHeader.AddressOfEntryPoint &&
			ntHeaders.OptionalHeader.AddressOfEntryPoint <= section.VirtualAddress + section.SizeOfRawData
			)
		{
			executableSection = i;
			break;
		}
	}

	Disassembly::byte* execSection = (Disassembly::byte*)dll + sectionHeaders[executableSection].PointerToRawData;
	size_t execSize = sectionHeaders[executableSection].SizeOfRawData;

	/*
		Disassemble it
	*/
	Disassembly::Disassembler dis(execSection, execSize);
	for (auto& ins : dis.instructions)
	{
		//std::cout << ins->toString() << std::endl;
		//std::cout << ins->size << " byte(s) - " << (ins->valid ? "valid" : "invalid") << std::endl;
	}

	return 0;
}