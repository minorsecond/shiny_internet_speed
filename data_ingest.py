import psycopg2
import csv
import os

print("connecting to 192.168.3.65")
conn = psycopg2.connect("host=192.168.3.65 dbname=internetStatus user=rwardrup password=Rward0232")
cur = conn.cursor()

# Read CSV
with open("./data/speedtest_data.csv", 'r') as f:
  reader = csv.reader(f)
  for row in reader:
    print("Uploading {0}".format(row))
    #cur.copy_from(f, 'internetstatus', sep=',')
    cur.execute(
      "INSERT INTO internetstatus VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s)",
      row
    )

print("Commiting transaction")
conn.commit()

print("Finished")
