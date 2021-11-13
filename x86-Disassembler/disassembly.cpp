#include "magic_enum.hpp"
#include "disassembly.hpp"
#include <iostream>
namespace L = Logging;

#define ENSURE_CAN_READ_BYTES(n) if (this->size + n > maxSize) \
{ \
	this->valid = false; \
	return; \
}
#define ENSURE_VALID() if (!this->valid) \
{ \
	return; \
}
Disassembly::Instruction::Instruction(const byte* bytes, const size_t& maxSize)
{
	L::log(L::LogLevel::DEBUG, "Creating new instruction.");

	/*
		Parse prefixes
	*/
	bool usingNewSchema = false;
	L::log(L::LogLevel::DEBUG, "byte = " + std::to_string((int)(unsigned char)bytes[this->size]));
	const InstructionSchema* pSchema = instructionSchemas + bytes[this->size++];
	while (pSchema->operatorSchema.prefix != Prefix::NONE)
	{
		L::log(L::LogLevel::DEBUG, "Had prefix: " + std::to_string((int)pSchema->operatorSchema.prefix));
		this->prefixes.set(pSchema->operatorSchema.prefix);

		ENSURE_CAN_READ_BYTES(1);
		L::log(L::LogLevel::DEBUG, "byte = " + std::to_string((int)(unsigned char)bytes[this->size]));
		pSchema = instructionSchemas + bytes[this->size++];
	}
	//std::cout << magic_enum::enum_name(pSchema->operatorSchema.mnemonic) << ": " << (int)pSchema->operatorSchema.group << std::endl;
	if (!InstructionSchema::isValid(*pSchema))
	{
		L::log(L::LogLevel::ERR, "Invalid Schema. Byte = " + std::to_string((int)bytes[this->size - 1]));
		this->valid = false;
		return;
	}
	else
	{
		L::log(L::LogLevel::DEBUG, "Valid Schema");
	}

	/*
		Parse group & ModR/M
	*/
	if (pSchema->operatorSchema.group != Group::NONE)
	{
		L::log(L::LogLevel::DEBUG, "Had group: " + std::string(magic_enum::enum_name(pSchema->operatorSchema.group)));

		this->setModRM(bytes, maxSize);
		ENSURE_VALID();

		// create new schema using group information
		usingNewSchema = true;
		const InstructionSchema& groupSchema = groupSchemas[(int)pSchema->operatorSchema.group - (int)Group::IMMEDIATE][(int)this->modRegRM->reg];
		InstructionSchema* newSchema = new InstructionSchema{
			groupSchema.operatorSchema.mnemonic,
			{ pSchema->operandSchema[0], pSchema->operandSchema[1], pSchema->operandSchema[2] }
		};

		// set unspecified fields
		for (size_t i = 0; i < MAX_OPERAND_COUNT; i++)
		{
			if (newSchema->operandSchema[i].addressingMethod == AddressingMethod::UNSPECIFIED)
			{
				if (groupSchema.operandSchema[i].addressingMethod == AddressingMethod::UNSPECIFIED)
				{
					this->valid = false;
					return;
				}
				else
				{
					newSchema->operandSchema[i].addressingMethod = groupSchema.operandSchema[i].addressingMethod;
				}
			}

			if (newSchema->operandSchema[i].operandSize == OperandSize::UNSPECIFIED)
			{
				if (groupSchema.operandSchema[i].operandSize == OperandSize::UNSPECIFIED)
				{
					this->valid = false;
					return;
				}
				else
				{
					newSchema->operandSchema[i].operandSize = groupSchema.operandSchema[i].operandSize;
				}
			}
		}

		pSchema = newSchema;
	}
	L::log(L::LogLevel::INFO, "mnemonic: " + std::string(magic_enum::enum_name(pSchema->operatorSchema.mnemonic)));

	while (this->operands.size() < MAX_OPERAND_COUNT && OperandSchema::isValid(pSchema->operandSchema[this->operands.size()]))
	{
		const auto& operandSchema = pSchema->operandSchema[this->operands.size()];
		this->operands.emplace_back(new Operand());
		const auto& operand = this->operands.at(this->operands.size() - 1);

		if (this->modRegRM.get() == nullptr && (
			operandSchema.addressingMethod == AddressingMethod::REG_OR_MEM ||
			operandSchema.addressingMethod == AddressingMethod::MEMORY ||
			operandSchema.addressingMethod == AddressingMethod::REGISTER ||
			operandSchema.addressingMethod == AddressingMethod::SEGMENT_REGISTER
			))
		{
			this->setModRM(bytes, maxSize);
		}
		else if (operandSchema.addressingMethod == AddressingMethod::IMMEDIATE)
		{
			switch (operandSchema.operandSize)
			{
			case OperandSize::BYTE:
				ENSURE_CAN_READ_BYTES(1);
				operand->imm8 = bytes[this->size++];
				break;
			case OperandSize::WORD:
				ENSURE_CAN_READ_BYTES(2);
				operand->imm16 = *(word*)(bytes + this->size);
				this->size += 2;
				break;
			case OperandSize::W_OR_D:
				if (this->prefixes.contains(Prefix::OPERAND))
				{
					ENSURE_CAN_READ_BYTES(2);
					operand->imm16 = *(word*)(bytes + this->size);
					this->size += 2;
				}
				else
				{
					ENSURE_CAN_READ_BYTES(4);
					operand->imm32 = *(dword*)(bytes + this->size);
					this->size += 4;
				}
				break;

			default:
				this->valid = false;
				return;
			}
		}
		else if (operandSchema.addressingMethod == AddressingMethod::RELATIVE)
		{
			switch (operandSchema.operandSize)
			{
			case OperandSize::BYTE:
				ENSURE_CAN_READ_BYTES(1);
				operand->disp8 = bytes[this->size++];
				break;
			case OperandSize::W_OR_D:
				if (this->prefixes.contains(Prefix::OPERAND))
				{
					ENSURE_CAN_READ_BYTES(2);
					operand->disp16 = *(word*)(bytes + this->size);
					this->size += 2;
				}
				else
				{
					ENSURE_CAN_READ_BYTES(4);
					operand->disp32 = *(dword*)(bytes + this->size);
					this->size += 4;
				}
				break;

			default:
				this->valid = false;
				return;
			}
		}
		else if (operandSchema.addressingMethod == AddressingMethod::MEM_OFFSET)
		{
			if (this->prefixes.contains(Prefix::OPERAND))
			{
				ENSURE_CAN_READ_BYTES(2);
				operand->disp16 = *(word*)(bytes + this->size);
				this->size += 2;
			}
			else
			{
				ENSURE_CAN_READ_BYTES(4);
				operand->disp32 = *(dword*)(bytes + this->size);
				this->size += 4;
			}
		}
		else if (operandSchema.addressingMethod == AddressingMethod::ACCESS)
		{
			ENSURE_CAN_READ_BYTES(4);
			operand->disp32 = *(dword*)(bytes + this->size);
			this->size += 4;
		}
	}


	if (usingNewSchema) delete pSchema;
}

void Disassembly::Instruction::setModRM(const byte* bytes, const size_t& maxSize)
{
	ENSURE_CAN_READ_BYTES(1);
	this->modRegRM = std::make_unique<ModRegRM>(bytes[this->size++]);

	// load SIB
	if (this->modRegRM->rm == RM::SIB && !this->prefixes.contains(Prefix::ADDRESS))
	{
		ENSURE_CAN_READ_BYTES(1);
		this->modRegRM->sib = std::make_unique<SIB>(bytes[this->size++]);

		if (this->modRegRM->mod == Mod::NO_DISP && this->modRegRM->sib->base == Base::EBP)
		{
			ENSURE_CAN_READ_BYTES(4);
			this->modRegRM->sib->disp32 = *(dword*)(bytes + this->size);
			this->size += 4;
		}
	}

	// load displacement
	if (this->modRegRM->mod == Mod::DISP)
	{
		switch (this->modRegRM->modSize)
		{
		case OperandSize::B:
			ENSURE_CAN_READ_BYTES(1);
			this->modRegRM->disp8 = bytes[this->size++];
			break;
		case OperandSize::V:
			if (this->prefixes.contains(Prefix::ADDRESS))
			{
				ENSURE_CAN_READ_BYTES(2);
				this->modRegRM->disp16 = *(word*)(bytes + this->size);
				this->size += 2;
			}
			else
			{
				ENSURE_CAN_READ_BYTES(4);
				this->modRegRM->disp32 = *(dword*)(bytes + this->size);
				this->size += 4;
			}
			break;
		default: // not possible, check ModRegRM constructor
			this->valid = false;
			return;
		}
	}
	else
	{
		if (this->prefixes.contains(Prefix::ADDRESS))
		{
			if (this->modRegRM->rm16 == RM::DISP)
			{

				ENSURE_CAN_READ_BYTES(2);
				this->modRegRM->disp16 = *(word*)(bytes + this->size);
				this->size += 2;
			}
		}
		else
		{
			if (this->modRegRM->rm == RM::DISP)
			{
				ENSURE_CAN_READ_BYTES(4);
				this->modRegRM->disp32 = *(dword*)(bytes + this->size);
				this->size += 4;
			}
		}
	}
}
#undef ENSURE_VALID
#undef ENSURE_CAN_READ_BYTES

Disassembly::Disassembler::Disassembler(const byte* bytes, const size_t& count)
{
	// just to avoid having to do a ton of re allocations
	// most instructions are less than 4 bytes, so we likely won't
	// over-allocate
	this->instructions.reserve(count / 4);

	for (size_t offset = 0, i = 0; offset < count; i++)
	{
		this->instructions.emplace_back(new Instruction(bytes + offset, count - offset));
		offset += this->instructions.at(i)->size;
	}
}
