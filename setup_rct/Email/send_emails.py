import os
import csv
import time
from datetime import datetime
import sys
import smtplib
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText
from email.mime.image import MIMEImage
from email.utils import COMMASPACE, formatdate, formataddr
from email.header import Header
from shutil import move


## set working directory
#os.chdir("/Users/Eli/workspace/Energy.Project/")
os.chdir("/home/CampusEnergy/Energy/")

Room, TreatmentType, Standing, ComparePercent, Email  = [], [], [], [], []

with open('./Email/email_detail.csv') as csvfile:
    l = csv.reader(csvfile, delimiter=',')
    header = next(l)
    for row in l:
        Room.append(row[0])
        TreatmentType.append(row[1])
        Standing.append(row[2])
        ComparePercent.append(row[3])
        Email.append(row[4])


# receiver = ["yyu50@illinois.edu"]
# receiver = ["yyu50@illinois.edu", "nogueir2@illinois.edu", "ecmyers@illinois.edu"]
SubjectLine = []
for s, c in zip(Standing, ComparePercent):
	if s == "Great":
		SubjectLine.append("You used " + c + "% less energy than efficient neighbors last week.☺☺")
	if s == "Good":
		SubjectLine.append("You used " + c + "% more energy than efficient neighbors last week.☺")
	if s == "Bad":
		SubjectLine.append("You used " + c + "% more energy than average neighbors last week.")

graph_folders = os.listdir("./Graphic.Report")
right_format_folders = []
import re
for f in graph_folders:
	right_format_folder = re.findall(r'(\d{6})', f)
	if len(right_format_folder) != 0:
		right_format_folders.append(right_format_folder[0])

if len(right_format_folders) > 1:
    raise AssertionError("More than one folder of graphs found.")
if len(right_format_folders) == 0:
    raise AssertionError("No folder of graphs found.")


graph_folder = "./Graphic.Report/" + right_format_folders[0] + "/"
graph_paths = []

for r in Room:
	graph_path = [p for p in os.listdir(graph_folder) if r in p]
	if len(graph_path) > 2:
		raise AssertionError("More than two graphs found for Room " + r)
	graph_paths.append([os.path.join(graph_folder, p) for p in graph_path])

################ Make Emails ##########################################
messages = []
sender_name = "Heating/Cooling Energy Report"
sender_addr = "no-reply@illinois.edu"
reply_to = "ACE-CampusEnergy@mx.uillinois.edu"
##### assert equal length here ####

for r, t, std, c, e, suj, g in zip(Room, TreatmentType, Standing, ComparePercent, Email, SubjectLine, graph_paths):
	if g is not None and isinstance(g, list):
		if len(g) > 2:
			print("More than 2 graphs found for Room " + r + "\n")

	msgRoot = MIMEMultipart('related') 
	msgRoot["From"] = formataddr((str(Header(sender_name, 'utf-8')), sender_addr))
	if isinstance(e, list):
		msgRoot['To'] = COMMASPACE.join(e)
	else:
		msgRoot['To'] = e
	# msgRoot['To'] = "nogueir2@illinois.edu"

	msgRoot['Date'] = formatdate(localtime=True)
	msgRoot['Subject'] = suj
	msgRoot['reply-to'] = reply_to


	if t == "Individual":
		main = "The following reports are based on thermostat setpoints in your room.\n"
	if t == "Cluster":
		main = "The following reports are based on thermostat setpoints in your suite.\n"

	# html_text = r + "<br>" + main.replace("\n", "<br>")
	html_text = main.replace("\n", "<br>")
	html_text += "<br>"
	if isinstance(g, list):
		html_text += '<img src="cid:image1" width="60%" align="left" /> \
					  <img src="cid:standing" width="35%" align="left" /> \
					  <img src="cid:image2" width="90%" /><br>'

	html_text += "<br>Energy saving tips:<br> \
				 &nbsp;&nbsp;&nbsp;&nbsp;In <b><font color=\"red\">HOT</font></b> days, turn your thermostat <b><font color=\"red\">UP</font></b> before going to sleep or when you leave the room.<br> \
				 &nbsp;&nbsp;&nbsp;&nbsp;In <b><font color=\"blue\">COLD</font></b> days, turn your thermostat <b><font color=\"blue\">DOWN</font></b> before going to sleep or when you leave the room.<br>"
	html_text += "<br>If you have any questions/concerns, or if you want to withdraw from this study, please contact Erica Myers (ecmyers@illinois.edu).<br>"
	msgText = MIMEText(html_text, 'html')
	msgRoot.attach(msgText)

	# Attach Graphs
	count = 1
	if g is None:
		print("No graphs found for Room " + r + "\n")
		raise
	else:
		for image in g:
			try:
				img_data = open(image, 'rb').read()
				msgImage = MIMEImage(img_data)
				img_id = '<image' + str(count) + '>'
				msgImage.add_header('Content-ID', img_id)
				msgRoot.attach(msgImage)
				count += 1
			except:
				print("Unable to open one of the attachments. Error:", sys.exc_info()[0])
				raise

	# Attach Standing Textbox
	if std is not None:
		try:
			standing_img = "./Email/Textbox/" + std + ".png"
			img_data = open(standing_img, 'rb').read()
			msgImage = MIMEImage(img_data)
			msgImage.add_header('Content-ID', '<standing>')
			msgRoot.attach(msgImage)
		except:
			print("Unable to open one of the attachments. Error:", sys.exc_info()[0])
			raise

	messages.append(msgRoot.as_string())

sent_dir = "./Graphic.Report/sent/" + right_format_folders[0]
if not os.path.exists(sent_dir):
    os.makedirs(sent_dir)

################ Send Emails ##########################################
#server = "outbound-relays.techservices.illinois.edu:25"
server = "incoming-relays.illinois.edu:25"
total_count = 0
while total_count < len(Room):
	while True:
		try:
			with smtplib.SMTP(server) as s:
				print("Connection Established.\n")
				for count in range(50):
					while total_count < len(Room):
						try: 
							if Email[total_count] and Email[total_count] != "NA" and Email[total_count] != "":
								s.sendmail(sender_addr, Email[total_count], messages[total_count])
#								s.sendmail(sender_addr, "ACE-CampusEnergy@mx.uillinois.edu", messages[total_count])
#								s.sendmail(sender_addr, "nogueir2@illinois.edu", messages[total_count])
								print("Email has been sent to " + Email[total_count] + " of Room " + Room[total_count] + ".\n")
							if total_count+1 == len(Room) or Room[total_count+1] != Room[total_count]:
								for g in graph_paths[total_count]:
									path, file_name = os.path.split(g)
									move(g, os.path.join(sent_dir, file_name))
							total_count += 1
						except:
							print("Unable to send the email to " + Email[total_count] + " of Room " + Room[total_count] + ".\nError:", sys.exc_info()[0])
							continue
						break
				s.close()
				print("Connection Closed.\n")
		except:
			print("Unable to connect to server.\nError:", sys.exc_info()[0])
			time.sleep(5)
			continue
		break

