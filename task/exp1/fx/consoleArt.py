
def artPrint(text): 
	from PIL import Image, ImageFont, ImageDraw

	font = ImageFont.truetype('arialbd.ttf', 15) #load the font
	size = font.getsize(text)  #calc the size of text in pixels
	image = Image.new('1', size, 1)  #create a b/w image
	draw = ImageDraw.Draw(image)
	draw.text((0, 0), text, font=font) #render the text to the bitmap

	def mapBitToChar(im, col, row):
	    if im.getpixel((col, row)): return ' '
	    else: return '#'

	for r in range(size[1]):
	    print(''.join([mapBitToChar(image, c, r) for c in range(size[0])]))

artPrint('test')