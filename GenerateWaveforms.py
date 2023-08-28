def generateWaveform2():
	p4 = 1
	p5 = 1
	output = ""

	while len(output) < 465:
		bit2 = 1 if p5 & 0x04 else 0
		bit4 = 1 if p5 & 0x10 else 0
		p5 = (p5 << 1) | (bit2 ^ bit4)

		if p5 & 0x0F == 0x08:
			bit2 = 1 if p4 & 0x04 else 0
			bit3 = 1 if p4 & 0x08 else 0
			p4 = (p4 << 1) | (bit2 ^ bit3)

		output = output + str((p4 >> 3) & 0x01)

	print(output)

def generateWaveform3():
	p4 = 1
	p5 = 1
	output = ""

	while len(output) < 465:
		bit2 = 1 if p5 & 0x04 else 0
		bit4 = 1 if p5 & 0x10 else 0
		p5 = (p5 << 1) | (bit2 ^ bit4)

		if p5 & 0x10 == 0x10:
			bit2 = 1 if p4 & 0x04 else 0
			bit3 = 1 if p4 & 0x08 else 0
			p4 = (p4 << 1) | (bit2 ^ bit3)

		output = output + str((p4 >> 3) & 0x01)

	print(output)

def generateWaveform8():
	p9 = 1
	output = ""

	while len(output) < 511:
		bit4 = 1 if p9 & 0x10 else 0
		bit8 = 1 if p9 & 0x100 else 0
		p9 = (p9 << 1) | (bit4 ^ bit8)
		output = output + str((p9 >> 8) & 0x01)
	print(output)

generateWaveform8()