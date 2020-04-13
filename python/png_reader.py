from PIL import Image

f = input("Filename (without the png): ")
try:
    im = Image.open(f + ".png")
except:
    print("Bad File")
    exit()

out = open(f + ".txt", "w")

w, h = im.size
data = list(im.getdata())

x, y = 0, 0

for pixel in data:
    a = "255"
    try:
        r, g, b, a = map(str, pixel)
    except:
        r, g, b = map(str, pixel)
    print(r, g, b, a, sep=",", end="")
    out.write(r+","+g+","+b+","+a)
    if x == (w-1):
        x = 0
        y += 1
        print()
        out.write("\n")
    else:
        x += 1
        print(" ", end="")
        out.write(" ")

out.close()
