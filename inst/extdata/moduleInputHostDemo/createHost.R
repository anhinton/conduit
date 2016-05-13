### create an XML file describing a module vagrantHost
library(XML)
xml <- newXMLNode(name = "vagrant",
                  attrs = list(
                      vagrantfile = "~/vagrant/vagrant-conduit/Vagrantfile"))
xml <- newXMLDoc(node = xml)
saveXML(xml, "vagrantHost.xml")
