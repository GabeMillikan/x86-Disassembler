#pragma once
#include <cstdint>
#include <memory>
#include <vector>
#include <string>
#include <iostream>

#ifdef _WINDOWS_
#error Please include windows AFTER including disassembly.hpp
#endif // WINDOWS


namespace Logging {
	// debug, info, notice, warn, error, crit, alert, emerg
	// correspond to Apache log levels
	enum class LogLevel {
		DEBUG = 0,
		INFO,
		NOTICE,
		WARN,
		ERROR,
		CRITICAL,
		ALERT,
		EMERGENCY,


		// aliases
		ERR = ERROR,
		CRIT = CRITICAL,
		EMERG = EMERGENCY
	};
	constexpr LogLevel activeLogLevel = LogLevel::WARN;

	template <typename T>
	constexpr inline void log(const LogLevel& ll, const T& log, const char* end = "\n")
	{
		if  (activeLogLevel <= ll)
		{
			std::cout << "[" << magic_enum::enum_name(ll) << "]: " << log << end;
		}
	}
}

namespace Disassembly {
	typedef uint8_t byte;
	typedef uint16_t word;
	typedef uint32_t dword;

	constexpr size_t MAX_OPERAND_COUNT = 3;

	// enum utility (CAUTION, ONLY WORKS WITH ENUMS ON RANGE [0, 63]
	template <class E>
	class EnumBitField {
		uint64_t field = 0;
		inline uint64_t toFlag(E field) { return (uint64_t)1 << (uint64_t)(field); }

	public:
		inline bool contains(E field) { return this->field & this->toFlag(field); }
		inline void set(E field) { this->field |= this->toFlag(field); }
		inline void unset(E field) { this->field &= ~(this->toFlag(field)); }
	};

	// operator enums
	enum class Prefix {
		// no operator
		_ = 0,
		NONE = _,

		ES, // E Segment Register Prefix
		CS, // C Segment Register Prefix
		SS, // S Segment Register Prefix
		DS, // D Segment Register Prefix
		FS, // F Segment Register Prefix
		GS, // G Segment Register Prefix
		OPERAND, // Operand Size Prefix
		ADDRESS, // Address Size Prefix
		LOCK, // Lock Prefix
		REPNE, // Repeat Not Equal Prefix
		REPE, // Repeat Equal Prefix
	};
	enum class Group {
		// no operator
		_ = 0,
		NONE = _,

		IMMEDIATE, // arithmetic operations
		SHIFT, // shift/rotate operations
		UNARY, // multiplication, division, test, negate operations
		FOUR, // increment, decrement
		FIVE, // increment, decrement, call, jump, push operations
		TWOBYTE, // two byte operations
	};
	enum class Mnemonic {
		// no operator
		_ = 0,
		NONE = _,

		/* Arithmetic Instructions */

		ADD,    // Add 
		SUB,    // Subtract
		MUL,    // Unsigned multiply
		DIV,    // Unsigned divide
		ADC,    // Add with carry
		SBB,    // Integer subtraction with borrow
		IMUL,   // Signed multiply
		IDIV,   // Signed divide
		INC,    // Increment by 1
		DEC,    // Decrement by 1

		/* Logical Instructions */

		NOT,    // One's complement negation
		NEG,    // Two's complement negation
		AND,    // Logical AND
		OR,     // Logical OR
		XOR,    // Logical exclusive OR

		/* Shift Instructions */

		ROL,    // Rotate left
		ROR,    // Rotate right
		RCL,    // Rotate left with carry
		RCR,    // Rotate right with carry
		SHL,    // Shift logical left
		SHR,    // Shift logical right
		SAL,    // Shift arithmetic left
		SAR,    // Shift arithmetic right

		/* Push Instructions */

		PUSH,   // Push word, doubleword, or quadword onto the stack
		PUSHA,  // Push all general-purpose registers
		PUSHAD, // Push all general-purpose registers
		PUSHF,  // Push FLAGS register onto the stack
		PUSHD,  // Push EFLAGS register onto the stack
		PUSHQ,  // Push RFLAGS reigster onto the stack

		/* Pop Instructions */

		POP,    // Pop a value from the stack
		POPA,   // Pop all general-purpose registers
		POPAD,  // Pop all general-purpose registers
		POPF,   // Pop stack into FLAGS register
		POPD,   // Pop stack into EFLAGS register
		POPQ,   // Pop stack into RFLAGS register

		/* Move Instructions */

		XCHG,   // Exchange register/memory with register
		MOV,    // Move
		MOVS,   // Move data from string to string
		MOVSB,  // Move data from string to string
		MOVSW,  // Move data from string to string
		MOVSD,  // Move data from string to string
		MOVSQ,  // Move data from string to string

		/* Jump Instructions */

		JMP,    // Jump
		JA,     // Jump if above (CF = 0 and ZF = 0)
		JAE,    // Jump if above or equal (CF = 0)
		JB,     // Jump if below (CF = 1)
		JBE,    // Jump if below or equal (CF = 1 or ZF = 1)
		JC,     // Jump if carry (CF = 1)
		JE,     // Jump if equal (ZF = 1)
		JG,     // Jump if greater (ZF = 0 and SF = OF)
		JGE,    // Jump if greater or equal (SF = OF)
		JL,     // Jump if less (SF != OF)
		JLE,    // Jump if less or equal (ZF = 1 or SF != OF)
		JNA,    // Jump if not above (CF = 1 or ZF = 1)
		JNAE,   // Jump if not above or equal (CF = 1)
		JNB,    // Jump if not below (CF = 0)
		JNBE,   // Jump if not below or equal (CF = 0 and ZF = 0)
		JNC,    // Jump if if not carry (CF = 0)
		JNE,    // Jump if not equal (ZF = 0)
		JNG,    // Jump if not greater (ZF = 1 or SF != OF)
		JNGE,   // Jump if not greater or equal (SF != OF)
		JNL,    // Jump if not less (SF = OF)
		JNLE,   // Jump if not less or equal (ZF = 0 and SF = OF)
		JNO,    // Jump if not overflow (OF = 0)
		JNP,    // Jump if not parity (PF = 0)
		JNS,    // Jump if not sign (SF = 0)
		JNZ,    // Jump if not zero (ZF = 0)
		JO,     // Jump if overflow (OF = 1)
		JP,     // Jump if parity (PF = 1)
		JPE,    // Jump if parity even (PF = 1)
		JPO,    // Jump if parity odd (PF = 0)
		JS,     // Jump if sign (SF = 1)
		JZ,     // Jump if zero (ZF = 1)
		JCXZ,   // Jump if CX register is 0
		JECXZ,  // Jump if ECX register is 0

		/* Procedure Instructions */

		CALL,   // Call procedure
		RET,    // Return from procedure
		ENTER,  // Make stack frame for procedure parameters
		LEAVE,  // High level prodecure exit

		/* Interrupt Instructions */

		INT,    // Call to interrupt prodedure
		INTO,   // Call to interrupt procedure
		IRET,   // Interrupt return
		IRETD,  // Interrupt return
		IRETQ,  // Interrupt return

		/* Input Instructions */

		IN,     // Input from port
		INS,    // Input from port to string
		INSB,   // Input from port to string
		INSW,   // Input from port to string
		INSD,   // Input from port to string

		/* Output Instructions */

		OUT,    // Output to port
		OUTS,   // Output string to port
		OUTSB,  // Output string to port
		OUTSW,  // Output string to port
		OUTSD,  // Output string to port

		/* Conversion Instructions */

		CBW,    // Convert byte to word
		CWDE,   // Convert word to doubleword
		CDQE,   // Convert doubleword to quadword
		CWD,    // Convert word to doubleword
		CDQ,    // Convert doubleword to quadword
		CQO,    // Convert quadword to double-quadword

		/* Loop Instructions */

		LOOPNE, // Decrement count, jump if count != 0 and ZF = 0
		LOOPNZ, // Decrement count, jump if count != 0 and ZF = 0
		LOOPE,  // Decrement count, jump if count != 0 and ZF = 1
		LOOPZ,  // Decrement count, jump if count != 0 and ZF = 1
		LOOP,   // Decrement count, jump if count != 0

		/* ASCII Adjust Instructions */

		AAM,    // ASCII adjust AX after multiply
		AAD,    // ASCII adjust AX before division
		AAA,    // ASCII adjust after addition
		AAS,    // ASCII adjust AL after subtraction

		/* Decimal Adjust Instructions */

		DAA,    // Decimal adjust AL after addition
		DAS,    // Decimal adjust AL after subtraction

		/* Compare Instructions */

		TEST,   // Logical compare
		CMP,    // Compare two operands
		CMPS,   // Compare string operands
		CMPSB,  // Compare string operands
		CMPSW,  // Compare string operands
		CMPSD,  // Compare string operands

		/* Flag Instructions */

		CMC,    // Complement carry flag
		CLC,    // Clear carry flag
		STC,    // Set carry flag
		CLI,    // Clear interrupt flag
		STI,    // Set interrupt flag
		CLD,    // Clear direction flag
		STD,    // Set direction flag

		/* Load String Instructions */

		LODS,   // Load string
		LODSB,  // Load string
		LODSW,  // Load string
		LODSD,  // Load string
		LODSQ,  // Load string

		/* Scan String Instructions */

		SCAS,   // Scan string
		SCASB,  // Scan string
		SCASW,  // Scan string
		SCASD,  // Scan string
		SCASQ,  // Scan string

		/* Store String Instructions */

		STOS,   // Store string
		STOSB,  // Store string
		STOSW,  // Store string
		STOSD,  // Store string
		STOSQ,  // Store string

		/* Obstruction Instructions */

		HLT,    // Halt
		WAIT,   // Wait
		FWAIT,  // Wait
		NOP,    // No operation
		PAUSE,  // Spin loop hint

		/* Pointer Instructions */

		LES,    // Load far pointer
		LDS,    // Load far pointer

		/* Table Instructions */

		XLAT,   // Table look-up translation
		XLATB,  // Table look-up translation

		/* FLAGS Instructions */

		SAHF,   // Store AH register into FLAGS
		LAHF,   // Load FLAGS into AH		

		/* Effective Address Instruction*/

		LEA,    // Load effective address

		/* Array Instruction */

		BOUND,  // Check array index against bounds

		/* Segment Instruction */

		ARPL,   // Adjust RPL field of segment selector
	};

	// operand enums
	enum class OperandSize {
		// no size
		_ = 0,
		NONE = _,

		// specified elsewhere
		$,
		UNSPECIFIED = $,

		// one byte
		B,
		BYTE = B,

		// two bytes
		W,
		WORD = W,

		// four bytes
		D,
		DWORD = D,

		// two or four bytes, depending on prefixes
		V,
		W_OR_D = V,

		// 1, 2, or 4, depending on prefixes
		U,
		B_OR_W_OR_D = U,

		// two words or dwords in memory, depending on prefixes
		A,
		TWO_V = A,

		// 4 byte pointer
		P,
		POINTER = P,
	};
	enum class AddressingMethod {
		// no operand
		_ = 0,
		NONE = _,

		// specified elsewhere
		$,
		UNSPECIFIED = $,

		// direct access [segment]:[offset]
		A,
		ACCESS = A,

		// relative offset
		J,
		RELATIVE = J,

		// memory offset relative to segment base
		O,
		MEM_OFFSET = O,

		// ModR/M specifies a general register or memory address
		E,
		REG_OR_MEM = E,

		// ModR/M specifies a memory address
		M,
		MEMORY = M,

		// ModR/M reg field specifies a general register
		G,
		REGISTER = G,

		// ModR/M reg field specifies a segment register
		S,
		SEGMENT_REGISTER = S,

		// immediate data
		I,
		IMMEDIATE = I,

		// Memory addressed by DS:rSI register pair
		X,
		FIXED_DS = X,

		// Memory addressed by ES:rDI register pair
		Y,
		FIXED_ES = Y,
	};
	enum class GeneralRegister {
		// no operand
		_ = 0,
		NONE = _,

		// specified elsewhere
		$,
		UNSPECIFIED = $,

		// registers
		A,
		C,
		D,
		B,
		SP,
		BP,
		SI,
		DI,
	};
	enum class SegmentRegister {
		// no operand
		_ = 0,
		NONE = _,

		// specified elsewhere
		$,
		UNSPECIFIED = $,

		// registers
		ES,
		CS,
		SS,
		DS,
		FS,
		GS,
	};
	enum class Constant {
		// no operand
		_ = 0,
		NONE = _,

		// specified elsewhere
		$,
		UNSPECIFIED = $,

		// used for Group::SHIFT
		ONE,
	};

	// modR/M enums
	enum class Mod {
		NO_DISP = 0,
		DISP,
		REG
	};
	enum class Reg {
		A = 0,
		C,
		D,
		B,
		SP,
		BP,
		SI,
		DI,
	};
	enum class RM {
		// specified elsewhere (used only in optional rm16 field)
		$,
		UNSPECIFIED = $,

		A, // [EAX] or [BX + SI] (16-bit)
		C, // [ECX] or [BX + DI] (16-bit)
		D, // [EDX] or [BP + SI] (16-bit)
		B, // [EBX] or [BP + DI] (16-bit)
		SP, // [ESP] or [SI] (16-bit)
		BP, // [EBP] or [DI] (16-bit)
		SI, // [ESI] or [BP] (16-bit)
		DI, // [EDI] or [BX] (16-bit)

		DISP, // Displacement (16-bit or 32-bit)

		SIB // The SIB byte is used
	};

	// SIB enums
	enum class Scale {
		ONE = 0,
		TWO,
		FOUR,
		EIGHT
	};
	enum class Index {
		EAX = 0,
		ECX,
		EDX,
		EBX,
		_, // ESP cannot be used
		EBP,
		ESI,
		EDI
	};
	enum class Base {
		EAX = 0,
		ECX,
		EDX,
		EBX,
		ESP,
		EBP, // EBP if MOD = 01 or MOD = 10, or 4 byte Displacement if MOD = 00
		ESI,
		EDI
	};

	// schemas
	struct OperatorSchema {
		Prefix prefix = Prefix::NONE;
		Group group = Group::NONE;
		Mnemonic mnemonic = Mnemonic::NONE;

		constexpr OperatorSchema() {
			this->prefix = Prefix::NONE;
			this->group = Group::NONE;
			this->mnemonic = Mnemonic::NONE;
		};
		constexpr OperatorSchema(Prefix prefix) : OperatorSchema() {
			this->prefix = prefix;
		};
		constexpr OperatorSchema(Group group) : OperatorSchema() {
			this->group = group;
		};
		constexpr OperatorSchema(Mnemonic mnemonic) : OperatorSchema() {
			this->mnemonic = mnemonic;
		};

		inline static bool isValid(const OperatorSchema& os) {
			if (os.prefix != Prefix::NONE)
				return os.group == Group::NONE && os.mnemonic == Mnemonic::NONE;
			else if (os.group != Group::NONE)
				return os.prefix == Prefix::NONE && os.mnemonic == Mnemonic::NONE;
			else if (os.mnemonic != Mnemonic::NONE)
				return os.group == Group::NONE && os.prefix == Prefix::NONE;

			return false;
		}
	};
	struct OperandSchema {
		AddressingMethod addressingMethod = AddressingMethod::NONE;
		GeneralRegister generalRegister = GeneralRegister::NONE;
		SegmentRegister segmentRegister = SegmentRegister::NONE;
		Constant constant = Constant::NONE;

		OperandSize operandSize = OperandSize::NONE;

		constexpr OperandSchema() {};
		constexpr OperandSchema(AddressingMethod addressingMethod, OperandSize operandSize) : addressingMethod(addressingMethod), operandSize(operandSize) {};
		constexpr OperandSchema(GeneralRegister generalRegister, OperandSize operandSize) : generalRegister(generalRegister), operandSize(operandSize) {};
		constexpr OperandSchema(SegmentRegister segmentRegister, OperandSize operandSize) : segmentRegister(segmentRegister), operandSize(operandSize) {};
		constexpr OperandSchema(Constant constant, OperandSize operandSize) : constant(constant), operandSize(operandSize) {};

		constexpr static inline bool isValid(const OperandSchema& in)
		{
			return in.addressingMethod != AddressingMethod::NONE ||
				in.generalRegister != GeneralRegister::NONE ||
				in.segmentRegister != SegmentRegister::NONE ||
				in.constant != Constant::NONE;
		}
	};
	struct InstructionSchema {
		OperatorSchema operatorSchema;
		OperandSchema operandSchema[MAX_OPERAND_COUNT];

		inline static bool isValid(const InstructionSchema& os) {
			return OperatorSchema::isValid(os.operatorSchema);
		}
	};

	constexpr OperandSchema NO_OPERAND = { AddressingMethod::NONE, OperandSize::NONE };
	constexpr OperandSchema UNSPECIFIED_OPERAND = { AddressingMethod::UNSPECIFIED, OperandSize::UNSPECIFIED };
	constexpr InstructionSchema INVALID_INSTRUCTION = { OperatorSchema(), { NO_OPERAND, NO_OPERAND, NO_OPERAND } };

	// mapping from x86 byte to its meaning
	constexpr InstructionSchema instructionSchemas[256] = {
		// 0x00 - 0x0F
		{ Mnemonic::ADD,    { { AddressingMethod::E, OperandSize::B }, { AddressingMethod::G, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::ADD,    { { AddressingMethod::E, OperandSize::V }, { AddressingMethod::G, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::ADD,    { { AddressingMethod::G, OperandSize::B }, { AddressingMethod::E, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::ADD,    { { AddressingMethod::G, OperandSize::V }, { AddressingMethod::E, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::ADD,    { { GeneralRegister::A,  OperandSize::B }, { AddressingMethod::I, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::ADD,    { { GeneralRegister::A,  OperandSize::V }, { AddressingMethod::I, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::PUSH,   { { SegmentRegister::ES, OperandSize::_ }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::POP,    { { SegmentRegister::ES, OperandSize::_ }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::OR,     { { AddressingMethod::E, OperandSize::B }, { AddressingMethod::G, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::OR,     { { AddressingMethod::E, OperandSize::V }, { AddressingMethod::G, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::OR,     { { AddressingMethod::G, OperandSize::B }, { AddressingMethod::E, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::OR,     { { AddressingMethod::G, OperandSize::V }, { AddressingMethod::E, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::OR,     { { GeneralRegister::A,  OperandSize::B }, { AddressingMethod::I, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::OR,     { { GeneralRegister::A,  OperandSize::V }, { AddressingMethod::I, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::PUSH,   { { SegmentRegister::CS, OperandSize::_ }, NO_OPERAND, NO_OPERAND } },
		INVALID_INSTRUCTION,

		// 0x10 - 0x1F
		{ Mnemonic::ADC,    { { AddressingMethod::E, OperandSize::B }, { AddressingMethod::G, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::ADC,    { { AddressingMethod::E, OperandSize::V }, { AddressingMethod::G, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::ADC,    { { AddressingMethod::G, OperandSize::B }, { AddressingMethod::E, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::ADC,    { { AddressingMethod::G, OperandSize::V }, { AddressingMethod::E, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::ADC,    { { GeneralRegister::A,  OperandSize::B }, { AddressingMethod::I, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::ADC,    { { GeneralRegister::A,  OperandSize::V }, { AddressingMethod::I, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::PUSH,   { { SegmentRegister::SS, OperandSize::_ }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::POP,    { { SegmentRegister::SS, OperandSize::_ }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::SBB,    { { AddressingMethod::E, OperandSize::B }, { AddressingMethod::G, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::SBB,    { { AddressingMethod::E, OperandSize::V }, { AddressingMethod::G, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::SBB,    { { AddressingMethod::G, OperandSize::B }, { AddressingMethod::E, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::SBB,    { { AddressingMethod::G, OperandSize::V }, { AddressingMethod::E, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::SBB,    { { GeneralRegister::A,  OperandSize::B }, { AddressingMethod::I, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::SBB,    { { GeneralRegister::A,  OperandSize::V }, { AddressingMethod::I, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::PUSH,   { { SegmentRegister::DS, OperandSize::_ }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::POP,    { { SegmentRegister::DS, OperandSize::_ }, NO_OPERAND, NO_OPERAND } },

		// 0x20 - 0x2F
		{ Mnemonic::AND,    { { AddressingMethod::E, OperandSize::B }, { AddressingMethod::G, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::AND,    { { AddressingMethod::E, OperandSize::V }, { AddressingMethod::G, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::AND,    { { AddressingMethod::G, OperandSize::B }, { AddressingMethod::E, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::AND,    { { AddressingMethod::G, OperandSize::V }, { AddressingMethod::E, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::AND,    { { GeneralRegister::A,  OperandSize::B }, { AddressingMethod::I, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::AND,    { { GeneralRegister::A,  OperandSize::V }, { AddressingMethod::I, OperandSize::V }, NO_OPERAND } },
		{ Prefix::ES,       { NO_OPERAND, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::DAA,    { NO_OPERAND, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::SUB,    { { AddressingMethod::E, OperandSize::B }, { AddressingMethod::G, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::SUB,    { { AddressingMethod::E, OperandSize::V }, { AddressingMethod::G, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::SUB,    { { AddressingMethod::G, OperandSize::B }, { AddressingMethod::E, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::SUB,    { { AddressingMethod::G, OperandSize::V }, { AddressingMethod::E, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::SUB,    { { GeneralRegister::A,  OperandSize::B }, { AddressingMethod::I, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::SUB,    { { GeneralRegister::A,  OperandSize::V }, { AddressingMethod::I, OperandSize::V }, NO_OPERAND } },
		{ Prefix::CS,       { NO_OPERAND, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::DAS,    { NO_OPERAND, NO_OPERAND, NO_OPERAND } },

		// 0x30 - 0x3F
		{ Mnemonic::XOR,    { { AddressingMethod::E, OperandSize::B }, { AddressingMethod::G, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::XOR,    { { AddressingMethod::E, OperandSize::V }, { AddressingMethod::G, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::XOR,    { { AddressingMethod::G, OperandSize::B }, { AddressingMethod::E, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::XOR,    { { AddressingMethod::G, OperandSize::V }, { AddressingMethod::E, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::XOR,    { { GeneralRegister::A,  OperandSize::B }, { AddressingMethod::I, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::XOR,    { { GeneralRegister::A,  OperandSize::V }, { AddressingMethod::I, OperandSize::V }, NO_OPERAND } },
		{ Prefix::SS,       { NO_OPERAND, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::AAA,    { NO_OPERAND, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::CMP,    { { AddressingMethod::E, OperandSize::B }, { AddressingMethod::G, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::CMP,    { { AddressingMethod::E, OperandSize::V }, { AddressingMethod::G, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::CMP,    { { AddressingMethod::G, OperandSize::B }, { AddressingMethod::E, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::CMP,    { { AddressingMethod::G, OperandSize::V }, { AddressingMethod::E, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::CMP,    { { GeneralRegister::A,  OperandSize::B }, { AddressingMethod::I, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::CMP,    { { GeneralRegister::A,  OperandSize::V }, { AddressingMethod::I, OperandSize::V }, NO_OPERAND } },
		{ Prefix::DS,       { NO_OPERAND, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::AAS,    { NO_OPERAND, NO_OPERAND, NO_OPERAND } },

		// 0x40 - 0x4F
		{ Mnemonic::INC,    { { GeneralRegister::A,  OperandSize::V }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::INC,    { { GeneralRegister::C,  OperandSize::V }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::INC,    { { GeneralRegister::D,  OperandSize::V }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::INC,    { { GeneralRegister::B,  OperandSize::V }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::INC,    { { GeneralRegister::SP, OperandSize::V }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::INC,    { { GeneralRegister::BP, OperandSize::V }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::INC,    { { GeneralRegister::SI, OperandSize::V }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::INC,    { { GeneralRegister::DI, OperandSize::V }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::DEC,    { { GeneralRegister::A,  OperandSize::V }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::DEC,    { { GeneralRegister::C,  OperandSize::V }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::DEC,    { { GeneralRegister::D,  OperandSize::V }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::DEC,    { { GeneralRegister::B,  OperandSize::V }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::DEC,    { { GeneralRegister::SP, OperandSize::V }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::DEC,    { { GeneralRegister::BP, OperandSize::V }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::DEC,    { { GeneralRegister::SI, OperandSize::V }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::DEC,    { { GeneralRegister::DI, OperandSize::V }, NO_OPERAND, NO_OPERAND } },

		// 0x50 - 0x5F
		{ Mnemonic::PUSH,   { { GeneralRegister::A,  OperandSize::V }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::PUSH,   { { GeneralRegister::C,  OperandSize::V }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::PUSH,   { { GeneralRegister::D,  OperandSize::V }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::PUSH,   { { GeneralRegister::B,  OperandSize::V }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::PUSH,   { { GeneralRegister::SP, OperandSize::V }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::PUSH,   { { GeneralRegister::BP, OperandSize::V }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::PUSH,   { { GeneralRegister::SI, OperandSize::V }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::PUSH,   { { GeneralRegister::DI, OperandSize::V }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::POP,    { { GeneralRegister::A,  OperandSize::V }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::POP,    { { GeneralRegister::C,  OperandSize::V }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::POP,    { { GeneralRegister::D,  OperandSize::V }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::POP,    { { GeneralRegister::B,  OperandSize::V }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::POP,    { { GeneralRegister::SP, OperandSize::V }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::POP,    { { GeneralRegister::BP, OperandSize::V }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::POP,    { { GeneralRegister::SI, OperandSize::V }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::POP,    { { GeneralRegister::DI, OperandSize::V }, NO_OPERAND, NO_OPERAND } },

		// 0x60 - 0x6F
		{ Mnemonic::PUSHA,  { NO_OPERAND, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::POPA,   { NO_OPERAND, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::BOUND,  { { AddressingMethod::G, OperandSize::V }, { AddressingMethod::M, OperandSize::A }, NO_OPERAND } },
		{ Mnemonic::ARPL,   { { AddressingMethod::E, OperandSize::W }, { AddressingMethod::G, OperandSize::W }, NO_OPERAND } },
		{ Prefix::FS,       { NO_OPERAND, NO_OPERAND, NO_OPERAND } },
		{ Prefix::GS,       { NO_OPERAND, NO_OPERAND, NO_OPERAND } },
		{ Prefix::OPERAND,  { NO_OPERAND, NO_OPERAND, NO_OPERAND } },
		{ Prefix::ADDRESS,  { NO_OPERAND, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::PUSH,   { { AddressingMethod::I, OperandSize::V }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::IMUL,   { { AddressingMethod::G, OperandSize::V }, { AddressingMethod::E, OperandSize::V }, { AddressingMethod::I, OperandSize::V } } },
		{ Mnemonic::PUSH,   { { AddressingMethod::I, OperandSize::B }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::IMUL,   { { AddressingMethod::G, OperandSize::V }, { AddressingMethod::E, OperandSize::V }, { AddressingMethod::I, OperandSize::B } } },
		{ Mnemonic::INS,    { { AddressingMethod::Y, OperandSize::B }, { GeneralRegister::D,  OperandSize::W }, NO_OPERAND } },
		{ Mnemonic::INS,    { { AddressingMethod::Y, OperandSize::V }, { GeneralRegister::D,  OperandSize::W }, NO_OPERAND } },
		{ Mnemonic::OUTS,   { { GeneralRegister::D,  OperandSize::W }, { AddressingMethod::X, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::OUTS,   { { GeneralRegister::D,  OperandSize::W }, { AddressingMethod::X, OperandSize::V }, NO_OPERAND } },

		// 0x70 - 0x7F
		{ Mnemonic::JO,     { { AddressingMethod::J, OperandSize::B }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::JNO,    { { AddressingMethod::J, OperandSize::B }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::JB,     { { AddressingMethod::J, OperandSize::B }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::JNB,    { { AddressingMethod::J, OperandSize::B }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::JZ,     { { AddressingMethod::J, OperandSize::B }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::JNZ,    { { AddressingMethod::J, OperandSize::B }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::JBE,    { { AddressingMethod::J, OperandSize::B }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::JNBE,   { { AddressingMethod::J, OperandSize::B }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::JS,     { { AddressingMethod::J, OperandSize::B }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::JNS,    { { AddressingMethod::J, OperandSize::B }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::JP,     { { AddressingMethod::J, OperandSize::B }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::JNP,    { { AddressingMethod::J, OperandSize::B }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::JL,     { { AddressingMethod::J, OperandSize::B }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::JNL,    { { AddressingMethod::J, OperandSize::B }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::JLE,    { { AddressingMethod::J, OperandSize::B }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::JNLE,   { { AddressingMethod::J, OperandSize::B }, NO_OPERAND, NO_OPERAND } },

		// 0x80 - 0x8F
		{ Group::IMMEDIATE, { { AddressingMethod::E, OperandSize::B }, { AddressingMethod::I, OperandSize::B }, NO_OPERAND } },
		{ Group::IMMEDIATE, { { AddressingMethod::E, OperandSize::V }, { AddressingMethod::I, OperandSize::V }, NO_OPERAND } },
		{ Group::IMMEDIATE, { { AddressingMethod::E, OperandSize::B }, { AddressingMethod::I, OperandSize::B }, NO_OPERAND } },
		{ Group::IMMEDIATE, { { AddressingMethod::E, OperandSize::V }, { AddressingMethod::I, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::TEST,   { { AddressingMethod::E, OperandSize::B }, { AddressingMethod::G, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::TEST,   { { AddressingMethod::E, OperandSize::V }, { AddressingMethod::G, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::XCHG,   { { AddressingMethod::E, OperandSize::B }, { AddressingMethod::G, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::XCHG,   { { AddressingMethod::E, OperandSize::V }, { AddressingMethod::G, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::MOV,    { { AddressingMethod::E, OperandSize::B }, { AddressingMethod::G, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::MOV,    { { AddressingMethod::E, OperandSize::V }, { AddressingMethod::G, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::MOV,    { { AddressingMethod::G, OperandSize::B }, { AddressingMethod::E, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::MOV,    { { AddressingMethod::G, OperandSize::V }, { AddressingMethod::E, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::MOV,    { { AddressingMethod::E, OperandSize::V }, { AddressingMethod::S, OperandSize::W }, NO_OPERAND } },
		{ Mnemonic::LEA,    { { AddressingMethod::G, OperandSize::V }, { AddressingMethod::M, OperandSize::_ }, NO_OPERAND } },
		{ Mnemonic::MOV,    { { AddressingMethod::S, OperandSize::W }, { AddressingMethod::E, OperandSize::W }, NO_OPERAND } },
		{ Mnemonic::POP,    { { AddressingMethod::E, OperandSize::V }, NO_OPERAND, NO_OPERAND } },

		// 0x90 - 0x9F
		{ Mnemonic::XCHG,   { { GeneralRegister::A,  OperandSize::V }, { GeneralRegister::A,  OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::XCHG,   { { GeneralRegister::A,  OperandSize::V }, { GeneralRegister::C,  OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::XCHG,   { { GeneralRegister::A,  OperandSize::V }, { GeneralRegister::D,  OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::XCHG,   { { GeneralRegister::A,  OperandSize::V }, { GeneralRegister::B,  OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::XCHG,   { { GeneralRegister::A,  OperandSize::V }, { GeneralRegister::SP, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::XCHG,   { { GeneralRegister::A,  OperandSize::V }, { GeneralRegister::BP, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::XCHG,   { { GeneralRegister::A,  OperandSize::V }, { GeneralRegister::SI, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::XCHG,   { { GeneralRegister::A,  OperandSize::V }, { GeneralRegister::DI, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::CBW,    { NO_OPERAND, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::CWD,    { NO_OPERAND, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::CALL,   { { AddressingMethod::A, OperandSize::P }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::WAIT,   { NO_OPERAND, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::PUSHF,  { NO_OPERAND, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::POPF,   { NO_OPERAND, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::SAHF,   { NO_OPERAND, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::LAHF,   { NO_OPERAND, NO_OPERAND, NO_OPERAND } },

		// 0xA0 - 0xAF
		{ Mnemonic::MOV,    { { GeneralRegister::A,  OperandSize::B }, { AddressingMethod::O, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::MOV,    { { GeneralRegister::A,  OperandSize::V }, { AddressingMethod::O, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::MOV,    { { AddressingMethod::O, OperandSize::B }, { GeneralRegister::A,  OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::MOV,    { { AddressingMethod::O, OperandSize::V }, { GeneralRegister::A,  OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::MOVS,   { { AddressingMethod::Y, OperandSize::B }, { AddressingMethod::X, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::MOVS,   { { AddressingMethod::Y, OperandSize::V }, { AddressingMethod::X, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::CMPS,   { { AddressingMethod::X, OperandSize::B }, { AddressingMethod::Y, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::CMPS,   { { AddressingMethod::X, OperandSize::V }, { AddressingMethod::Y, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::TEST,   { { GeneralRegister::A,  OperandSize::B }, { AddressingMethod::I, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::TEST,   { { GeneralRegister::A,  OperandSize::V }, { AddressingMethod::I, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::STOS,   { { AddressingMethod::Y, OperandSize::B }, { GeneralRegister::A,  OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::STOS,   { { AddressingMethod::Y, OperandSize::V }, { GeneralRegister::A,  OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::LODS,   { { GeneralRegister::A,  OperandSize::B }, { AddressingMethod::X, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::LODS,   { { GeneralRegister::A,  OperandSize::V }, { AddressingMethod::X, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::SCAS,   { { GeneralRegister::A,  OperandSize::B }, { AddressingMethod::Y, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::SCAS,   { { GeneralRegister::A,  OperandSize::V }, { AddressingMethod::Y, OperandSize::V }, NO_OPERAND } },

		// 0xB0 - 0xBF
		{ Mnemonic::MOV,    { { GeneralRegister::A,  OperandSize::B }, { AddressingMethod::I, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::MOV,    { { GeneralRegister::C,  OperandSize::B }, { AddressingMethod::I, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::MOV,    { { GeneralRegister::D,  OperandSize::B }, { AddressingMethod::I, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::MOV,    { { GeneralRegister::B,  OperandSize::B }, { AddressingMethod::I, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::MOV,    { { GeneralRegister::SP, OperandSize::B }, { AddressingMethod::I, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::MOV,    { { GeneralRegister::BP, OperandSize::B }, { AddressingMethod::I, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::MOV,    { { GeneralRegister::SI, OperandSize::B }, { AddressingMethod::I, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::MOV,    { { GeneralRegister::DI, OperandSize::B }, { AddressingMethod::I, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::MOV,    { { GeneralRegister::A,  OperandSize::V }, { AddressingMethod::I, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::MOV,    { { GeneralRegister::C,  OperandSize::V }, { AddressingMethod::I, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::MOV,    { { GeneralRegister::D,  OperandSize::V }, { AddressingMethod::I, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::MOV,    { { GeneralRegister::B,  OperandSize::V }, { AddressingMethod::I, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::MOV,    { { GeneralRegister::SP, OperandSize::V }, { AddressingMethod::I, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::MOV,    { { GeneralRegister::BP, OperandSize::V }, { AddressingMethod::I, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::MOV,    { { GeneralRegister::SI, OperandSize::V }, { AddressingMethod::I, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::MOV,    { { GeneralRegister::DI, OperandSize::V }, { AddressingMethod::I, OperandSize::V }, NO_OPERAND } },

		// 0xC0 - 0xCF
		{ Group::SHIFT,     { { AddressingMethod::E, OperandSize::B }, { AddressingMethod::I, OperandSize::B }, NO_OPERAND } },
		{ Group::SHIFT,     { { AddressingMethod::E, OperandSize::V }, { AddressingMethod::I, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::RET,    { { AddressingMethod::I, OperandSize::W }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::RET,    { NO_OPERAND, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::LES,    { { AddressingMethod::G, OperandSize::V }, { AddressingMethod::M, OperandSize::P }, NO_OPERAND } },
		{ Mnemonic::LDS,    { { AddressingMethod::G, OperandSize::V }, { AddressingMethod::M, OperandSize::P }, NO_OPERAND } },
		{ Mnemonic::MOV,    { { AddressingMethod::E, OperandSize::B }, { AddressingMethod::I, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::MOV,    { { AddressingMethod::E, OperandSize::V }, { AddressingMethod::I, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::ENTER,  { { AddressingMethod::I, OperandSize::W }, { AddressingMethod::I, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::LEAVE,  { NO_OPERAND, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::RET,    { { AddressingMethod::I, OperandSize::W }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::RET,    { NO_OPERAND, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::INT,    { NO_OPERAND, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::INT,    { { AddressingMethod::I, OperandSize::B }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::INTO,   { NO_OPERAND, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::IRET,   { NO_OPERAND, NO_OPERAND, NO_OPERAND } },

		// 0xD0 - 0xDF
		{ Group::SHIFT,     { { AddressingMethod::E, OperandSize::B }, { Constant::ONE,       OperandSize::_ }, NO_OPERAND } },
		{ Group::SHIFT,     { { AddressingMethod::E, OperandSize::V }, { Constant::ONE,       OperandSize::_ }, NO_OPERAND } },
		{ Group::SHIFT,     { { AddressingMethod::E, OperandSize::B }, { GeneralRegister::C,  OperandSize::B }, NO_OPERAND } },
		{ Group::SHIFT,     { { AddressingMethod::E, OperandSize::V }, { GeneralRegister::C,  OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::AAM,    { { AddressingMethod::I, OperandSize::B }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::AAD,    { { AddressingMethod::I, OperandSize::B }, NO_OPERAND, NO_OPERAND } },
		INVALID_INSTRUCTION,
		{ Mnemonic::XLAT,   { NO_OPERAND, NO_OPERAND, NO_OPERAND } },
		INVALID_INSTRUCTION,
		INVALID_INSTRUCTION,
		INVALID_INSTRUCTION,
		INVALID_INSTRUCTION,
		INVALID_INSTRUCTION,
		INVALID_INSTRUCTION,
		INVALID_INSTRUCTION,
		INVALID_INSTRUCTION,

		// 0xE0 - 0xEF
		{ Mnemonic::LOOPNE, { { AddressingMethod::J, OperandSize::B }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::LOOPE,  { { AddressingMethod::J, OperandSize::B }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::LOOP,   { { AddressingMethod::J, OperandSize::B }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::JECXZ,  { { AddressingMethod::J, OperandSize::B }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::IN,     { { GeneralRegister::A,  OperandSize::B }, { AddressingMethod::I, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::IN,     { { GeneralRegister::A,  OperandSize::V }, { AddressingMethod::I, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::OUT,    { { GeneralRegister::A,  OperandSize::B }, { AddressingMethod::I, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::OUT,    { { GeneralRegister::A,  OperandSize::V }, { AddressingMethod::I, OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::CALL,   { { AddressingMethod::J, OperandSize::V }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::JMP,    { { AddressingMethod::J, OperandSize::V }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::JMP,    { { AddressingMethod::A, OperandSize::P }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::JMP,    { { AddressingMethod::J, OperandSize::B }, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::IN,     { { GeneralRegister::A,  OperandSize::B }, { GeneralRegister::D,  OperandSize::W }, NO_OPERAND } },
		{ Mnemonic::IN,     { { GeneralRegister::A,  OperandSize::V }, { GeneralRegister::D,  OperandSize::W }, NO_OPERAND } },
		{ Mnemonic::OUT,    { { GeneralRegister::D,  OperandSize::W }, { GeneralRegister::A,  OperandSize::B }, NO_OPERAND } },
		{ Mnemonic::OUT,    { { GeneralRegister::D,  OperandSize::W }, { GeneralRegister::A,  OperandSize::V }, NO_OPERAND } },

		// 0xF0 - 0xFF
		{ Prefix::LOCK,     { NO_OPERAND, NO_OPERAND, NO_OPERAND } },
		INVALID_INSTRUCTION,
		{ Prefix::REPNE,    { NO_OPERAND, NO_OPERAND, NO_OPERAND } },
		{ Prefix::REPE,     { NO_OPERAND, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::HLT,    { NO_OPERAND, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::CMC,    { NO_OPERAND, NO_OPERAND, NO_OPERAND } },
		{ Group::UNARY,     { { AddressingMethod::E, OperandSize::B }, { AddressingMethod::$, OperandSize::B }, NO_OPERAND } },
		{ Group::UNARY,     { { AddressingMethod::E, OperandSize::V }, { AddressingMethod::$, OperandSize::V }, NO_OPERAND } },
		{ Mnemonic::CLC,    { NO_OPERAND, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::STC,    { NO_OPERAND, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::CLI,    { NO_OPERAND, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::STI,    { NO_OPERAND, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::CLD,    { NO_OPERAND, NO_OPERAND, NO_OPERAND } },
		{ Mnemonic::STD,    { NO_OPERAND, NO_OPERAND, NO_OPERAND } },
		{ Group::FOUR,      { { AddressingMethod::E, OperandSize::B }, NO_OPERAND, NO_OPERAND } },
		{ Group::FIVE,      { { AddressingMethod::$, OperandSize::$ }, NO_OPERAND, NO_OPERAND } }
	};

	constexpr InstructionSchema groupSchemas[5][8] = {
		// Group::IMMEDIATE
		{
			{ Mnemonic::ADD, { UNSPECIFIED_OPERAND, UNSPECIFIED_OPERAND, NO_OPERAND } },
			{ Mnemonic::OR,  { UNSPECIFIED_OPERAND, UNSPECIFIED_OPERAND, NO_OPERAND } },
			{ Mnemonic::ADC, { UNSPECIFIED_OPERAND, UNSPECIFIED_OPERAND, NO_OPERAND } },
			{ Mnemonic::SBB, { UNSPECIFIED_OPERAND, UNSPECIFIED_OPERAND, NO_OPERAND } },
			{ Mnemonic::AND, { UNSPECIFIED_OPERAND, UNSPECIFIED_OPERAND, NO_OPERAND } },
			{ Mnemonic::SUB, { UNSPECIFIED_OPERAND, UNSPECIFIED_OPERAND, NO_OPERAND } },
			{ Mnemonic::XOR, { UNSPECIFIED_OPERAND, UNSPECIFIED_OPERAND, NO_OPERAND } },
			{ Mnemonic::CMP, { UNSPECIFIED_OPERAND, UNSPECIFIED_OPERAND, NO_OPERAND } },
		},

		// Group::SHIFT
		{
			{ Mnemonic::ROL, { UNSPECIFIED_OPERAND, UNSPECIFIED_OPERAND, NO_OPERAND } },
			{ Mnemonic::ROR, { UNSPECIFIED_OPERAND, UNSPECIFIED_OPERAND, NO_OPERAND } },
			{ Mnemonic::RCL, { UNSPECIFIED_OPERAND, UNSPECIFIED_OPERAND, NO_OPERAND } },
			{ Mnemonic::RCR, { UNSPECIFIED_OPERAND, UNSPECIFIED_OPERAND, NO_OPERAND } },
			{ Mnemonic::SHL, { UNSPECIFIED_OPERAND, UNSPECIFIED_OPERAND, NO_OPERAND } },
			{ Mnemonic::SHR, { UNSPECIFIED_OPERAND, UNSPECIFIED_OPERAND, NO_OPERAND } },
			INVALID_INSTRUCTION,
			{ Mnemonic::SAR, { UNSPECIFIED_OPERAND, UNSPECIFIED_OPERAND, NO_OPERAND } },
		},

		// GROUP::UNARY
		{
			{ Mnemonic::TEST, { UNSPECIFIED_OPERAND, { AddressingMethod::I, OperandSize::$ }, NO_OPERAND } },
			INVALID_INSTRUCTION,
			{ Mnemonic::NOT,  { NO_OPERAND, NO_OPERAND, NO_OPERAND } },
			{ Mnemonic::NEG,  { NO_OPERAND, NO_OPERAND, NO_OPERAND } },
			{ Mnemonic::MUL,  { UNSPECIFIED_OPERAND, { GeneralRegister::A,  OperandSize::$ }, NO_OPERAND } },
			{ Mnemonic::IMUL, { UNSPECIFIED_OPERAND, { GeneralRegister::A,  OperandSize::$ }, NO_OPERAND } },
			{ Mnemonic::DIV,  { UNSPECIFIED_OPERAND, { GeneralRegister::A,  OperandSize::$ }, NO_OPERAND } },
			{ Mnemonic::IDIV, { UNSPECIFIED_OPERAND, { GeneralRegister::A,  OperandSize::$ }, NO_OPERAND } },
		},

		// Group::FOUR
		{
			{ Mnemonic::INC, { UNSPECIFIED_OPERAND, NO_OPERAND, NO_OPERAND } },
			{ Mnemonic::DEC, { UNSPECIFIED_OPERAND, NO_OPERAND, NO_OPERAND } },
			INVALID_INSTRUCTION,
			INVALID_INSTRUCTION,
			INVALID_INSTRUCTION,
			INVALID_INSTRUCTION,
			INVALID_INSTRUCTION,
			INVALID_INSTRUCTION,
		},

		// Group::FIVE
		{
			{ Mnemonic::INC,  { { AddressingMethod::E, OperandSize::V }, NO_OPERAND, NO_OPERAND } },
			{ Mnemonic::DEC,  { { AddressingMethod::E, OperandSize::V }, NO_OPERAND, NO_OPERAND } },
			{ Mnemonic::CALL, { { AddressingMethod::E, OperandSize::V }, NO_OPERAND, NO_OPERAND } },
			{ Mnemonic::CALL, { { AddressingMethod::E, OperandSize::P }, NO_OPERAND, NO_OPERAND } },
			{ Mnemonic::JMP,  { { AddressingMethod::E, OperandSize::V }, NO_OPERAND, NO_OPERAND } },
			{ Mnemonic::JMP,  { { AddressingMethod::M, OperandSize::P }, NO_OPERAND, NO_OPERAND } },
			{ Mnemonic::PUSH, { { AddressingMethod::E, OperandSize::V }, NO_OPERAND, NO_OPERAND } },
			INVALID_INSTRUCTION,
		}
	};

	// util structs
	struct SIB {
		Scale scale;
		Index index;
		Base base;

		dword disp32 = 0;

		inline SIB(const byte& b) {
			const byte hightwo = (b & 0b11000000) >> 6;
			const byte midthree = (b & 0b00111000) >> 3;
			const byte lowthree = b & 0b00000111;

			switch (hightwo)
			{
			default:
			case 0b00: this->scale = Scale::ONE; break;
			case 0b01: this->scale = Scale::TWO; break;
			case 0b10: this->scale = Scale::FOUR; break;
			case 0b11: this->scale = Scale::EIGHT; break;
			}

			this->index = (Index)midthree; // will be Index::_ for midthree = 0b100
			this->base = (Base)lowthree;
		}
	};
	struct ModRegRM {
		Mod mod;
		OperandSize modSize;

		Reg reg;
		OperandSize regSize;

		RM rm;
		RM rm16;
		OperandSize rmSize;

		std::unique_ptr<SIB> sib = nullptr;
		
		byte disp8 = 0;
		word disp16 = 0;
		dword disp32 = 0;

		inline ModRegRM(const byte& b) {
			const byte hightwo = (b & 0b11000000) >> 6;
			const byte midthree = (b & 0b00111000) >> 3;
			const byte lowthree = b & 0b00000111;

			// two high bits
			switch (hightwo)
			{
			default:
			case 0b00:
				this->mod = Mod::NO_DISP;
				this->modSize = OperandSize::NONE;

				this->rmSize = OperandSize::V;
				switch (lowthree) {
				default:
				case 0b000: this->rm = RM::A; this->rm16 = RM::A; break;
				case 0b001: this->rm = RM::C; this->rm16 = RM::C; break;
				case 0b010: this->rm = RM::D; this->rm16 = RM::D; break;
				case 0b011: this->rm = RM::B; this->rm16 = RM::B; break;
				case 0b100: this->rm = RM::SIB; this->rm16 = RM::SP; break;
				case 0b101: this->rm = RM::DISP; this->rm16 = RM::BP; break;
				case 0b110: this->rm = RM::SI; this->rm16 = RM::DISP; break;
				case 0b111: this->rm = RM::DI; this->rm16 = RM::DI; break;
				}
				break;
			case 0b01:
				this->mod = Mod::DISP;
				this->modSize = OperandSize::B;

				this->rmSize = OperandSize::V;
				switch (lowthree) {
				default:
				case 0b000: this->rm = RM::A; this->rm16 = RM::A; break;
				case 0b001: this->rm = RM::C; this->rm16 = RM::C; break;
				case 0b010: this->rm = RM::D; this->rm16 = RM::D; break;
				case 0b011: this->rm = RM::B; this->rm16 = RM::B; break;
				case 0b100: this->rm = RM::SIB; this->rm16 = RM::SP; break;
				case 0b101: this->rm = RM::BP; this->rm16 = RM::BP; break;
				case 0b110: this->rm = RM::SI; this->rm16 = RM::SI; break;
				case 0b111: this->rm = RM::DI; this->rm16 = RM::DI; break;
				}
				break;
			case 0b10:
				this->mod = Mod::DISP;
				this->modSize = OperandSize::V;

				this->rmSize = OperandSize::V;
				switch (lowthree) {
				default:
				case 0b000: this->rm = RM::A; this->rm16 = RM::A; break;
				case 0b001: this->rm = RM::C; this->rm16 = RM::C; break;
				case 0b010: this->rm = RM::D; this->rm16 = RM::D; break;
				case 0b011: this->rm = RM::B; this->rm16 = RM::B; break;
				case 0b100: this->rm = RM::SIB; this->rm16 = RM::SP; break;
				case 0b101: this->rm = RM::BP; this->rm16 = RM::BP; break;
				case 0b110: this->rm = RM::SI; this->rm16 = RM::SI; break;
				case 0b111: this->rm = RM::DI; this->rm16 = RM::DI; break;
				}
				break;
			case 0b11:
				this->mod = Mod::REG;
				this->modSize = OperandSize::UNSPECIFIED;

				this->rmSize = OperandSize::U;
				this->rm16 = RM::UNSPECIFIED;
				switch (lowthree) {
				default:
				case 0b000: this->rm = RM::A; break;
				case 0b001: this->rm = RM::C; break;
				case 0b010: this->rm = RM::D; break;
				case 0b011: this->rm = RM::B; break;
				case 0b100: this->rm = RM::SP; break;
				case 0b101: this->rm = RM::BP; break;
				case 0b110: this->rm = RM::SI; break;
				case 0b111: this->rm = RM::DI; break;
				}
				break;
			}

			// doesn't depend on anything else
			this->reg = (Reg)midthree;
			this->regSize = OperandSize::U;
		};
	};
	struct Operand {
		byte imm8 = 0;
		word imm16 = 0;
		dword imm32 = 0;

		byte disp8 = 0;
		word disp16 = 0;
		dword disp32 = 0;
	};
	struct Operator {

	};

	// main instruction
	struct Instruction {
	private:
		void setModRM(const byte* bytes, const size_t& maxSize);

	public:
		bool valid = true;
		size_t size = 0;

		EnumBitField<Prefix> prefixes;
		std::unique_ptr<ModRegRM> modRegRM = nullptr;
		std::unique_ptr<Operator> op = nullptr;
		std::vector<std::unique_ptr<Operand>> operands = {};
		
		Instruction(const byte* bytes, const size_t& maxSize);

		std::string toString();
	};

	// disassembler
	struct Disassembler {
		Disassembler(const byte* bytes, const size_t& count);

		std::vector<std::unique_ptr<Instruction>> instructions;
	};
}