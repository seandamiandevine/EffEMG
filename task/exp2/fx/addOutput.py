import csv 

def initCSV(file, header):
    '''
    Takes in a filename and writes header row to a .csv file.
    '''
    with open(file, 'w') as csvFile:
        writer = csv.writer(csvFile)
        writer.writerow(header)

def addOutput(file, output):
    '''
    Takes in a list and writes row to a .csv file with name file.
    '''
    with open(file, 'a') as data:
        writer = csv.writer(data)
        writer.writerow(output)