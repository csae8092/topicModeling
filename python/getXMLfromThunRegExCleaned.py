
# coding: utf-8

# In[9]:

import re, sys, urllib.request, time
import lxml.etree as ET


# In[10]:

url = 'http://thun-korrespondenz.uibk.ac.at:8080/exist/rest/db/files/thun/xml'


# In[11]:

try:
    u = urllib.request.urlopen(url)
except:
    print('something is wrong with this url')
    sys.exit(0)


# In[12]:

dom = ET.parse(u)


# In[13]:

resource = dom.xpath("//exist:resource/@name",
                    namespaces={'exist':
                               'http://exist.sourceforge.net/NS/exist'})


# In[14]:

for file in resource:
    fileUrl = url+"/"+file
    try:
        u = urllib.request.urlopen(fileUrl)
    except:
        print('something is wrong with this url')
        sys.exit(0)
    text = u.read().decode(u.headers.get_content_charset())
    #print(text)
    notePattern = re.compile(r'<note.*?</note>', re.DOTALL)
    text = re.sub(notePattern, "", text)
    pattern = re.compile(r'<.*</index>', re.DOTALL)
    text = re.sub(pattern," ", text)
    pattern = re.compile(r"<.*?>", re.DOTALL)
    text = re.sub(pattern," ", text)
	text = text.lower()
    #print(text)
    fileName = re.sub(r'.xml', ".txt", file)
    output = open(fileName, 'w', encoding="utf")
    output.write(text)
    output.close()
    time.sleep(5)


# In[ ]:



