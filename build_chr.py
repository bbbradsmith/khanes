#!/usr/bin/env python3

import PIL.Image
import math

#
# processes some image files into NES CHR data
#

def index_image(filename, palette):
    # load and convert image to indexed palette
    src = PIL.Image.open(filename)
    dst = PIL.Image.new("P",src.size,color=0)
    for y in range(src.size[1]):
        for x in range(src.size[0]):
            p = src.getpixel((x,y))
            mag = ((255**2)*3)+1
            mat = 0
            for i in range(len(palette)):
                m = sum([(a-b)**2 for (a,b) in zip(p,palette[i])])
                if m < mag: # better match
                    mat = i
                    mag = m
                    if m == 0: # perfect match
                        break
            dst.putpixel((x,y),mat)
    return dst

def print_img(im):
    # print indexed image (for debugging)
    print(im)
    for y in range(im.size[1]):
        s = "%3d:" % y
        for x in range(im.size[0]):
            s += "" + str(im.getpixel((x,y)))
        print(s)

def make_chr(im, horizontal=True):
    # convert indexed (0-3) image to CHR, as 8x8 tiles
    c = []
    wx, wy = im.size[0]//8, im.size[1]//8
    if not horizontal:
        wy, wx = wx, wy
    for ty in range(wy):
        for tx in range(wx):
            ox = tx*8
            oy = ty*8
            if not horizontal:
                oy, ox = ox, oy
            c0 = []
            c1 = []
            for y in range(8):
                r0 = 0
                r1 = 0
                for x in range(8):
                    p = im.getpixel((x+ox,y+oy))
                    r0 =  (p & 1)       | (r0 << 1)
                    r1 = ((p & 2) >> 1) | (r1 << 1)
                c0.append(r0)
                c1.append(r1)
            c = c + c0 + c1
    return c

def chr_to_rgb(c, palette, width=16, horizontal=True):
    # converts CHR data into an RGB image
    # using the given 4-colour palette.
    tiles = len(c) // 16
    columns = width
    rows = (tiles + (width-1)) // width
    if not horizontal:
        rows, columns = columns, rows
    img = PIL.Image.new("RGB",(width*8,rows*8),palette[0])
    for t in range(0,tiles):
        xo = (t % width) * 8
        yo = (t // width) * 8
        if not horizontal:
            yo, xo = xo, yo
        to = t * 16
        for y in range(8):
            p0 = c[to + 0 + y]
            p1 = c[to + 8 + y]
            for x in range(8):
                p = ((p0 >> 7) & 1) | ((p1 >> 6) & 2)
                img.putpixel((xo+x, yo+y), palette[p])
                p0 <<= 1
                p1 <<= 1
    return img

#
# sliding rotation of an image that decomposes into 8x8 tiles
#

def sliders(im):
    chr = []
    for ox in range(0,im.width,8):
        for oy in range(0,im.height):
            tim = im.crop((0,0,8,8))
            for y in range(0,8):
                for x in range(0,8):
                    src = im.getpixel((ox+x,(y-oy)%im.height))
                    tim.putpixel((x,y),src)
            chr += make_chr(tim)
    return chr

#
# output filenames
#

PALETTE = [ (0,0,0), (101,16,0), (255,134,95), (255,255,255) ]
chrout = "khan.chr"
chrdeb = "temp\\khan.chr.png"

base = index_image("khan.png",PALETTE)

chr0 = []
chr0 += make_chr(base.crop((0,64,64,80))) # text
chr0 += sliders(base.crop(( 0, 0,32,32))) # 32x32 head
chr0 += sliders(base.crop((32, 0,56,24))) # 24x24
chr0 += sliders(base.crop((56, 0,72,16))) # 16x16
chr0 += sliders(base.crop((72, 0,80, 8))) # 8x8
chr0 += [0] * (4096-len(chr0)) # padding to 4k

chr1 = []
chr1 += make_chr(base.crop((0,80,64,96)))
chr1 += sliders(base.crop(( 0,32,32,64)))
chr1 += sliders(base.crop((32,24,56,48)))

chr1 += sliders(base.crop((56,16,72,32)))
chr1 += sliders(base.crop((72, 8,80,16)))
chr1 += [0] * (4096-len(chr1))

chr_to_rgb(chr0+chr1, PALETTE, width=8).save(chrdeb)
print("debug: " + chrdeb)

open(chrout,"wb").write(bytes(chr0+chr1))
print("output: " + chrout)
