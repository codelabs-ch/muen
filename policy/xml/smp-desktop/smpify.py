#!/usr/bin/env python

import sys
from copy import deepcopy
from lxml import etree

if len(sys.argv) != 2:
    print sys.argv[0] + " <linux_cores>"
    sys.exit(1)

cores = int(sys.argv[1])
if cores < 2:
    print "Core count must be >= 2"
    sys.exit(1)
cores -= 1

dbgchannel_end = 4


def create_phys_events(events):
    for i in range(1, cores + 1):
        events.append(etree.Element("event", mode="switch",
                                    name="trap_to_sm_" + str(i)))
        events.append(etree.Element("event", mode="switch",
                                    name="resume_linux_" + str(i)))
        events.append(etree.Element("event", mode="async",
                                    name="serial_irq4_linux_" + str(i)))
        events.append(etree.Element("event", mode="self",
                                    name="timer_linux_" + str(i)))
        events.append(etree.Element("event", mode="asap",
                                    name="linux_smp_signal_sm_"
                                    + str(i).zfill(2)))


def create_phys_channels(events):
    for i in range(1, cores + 1):
        events.append(
                etree.Element("channel", name="debuglog_subject"
                              + str(dbgchannel_end + i), size="$logchannel_size"))


def create_lnx_resources(subject):
    vmcall = subject.xpath("events/source/group[@name='vmcall']")[0]
    for i in range(1, cores + 1):
        etree.SubElement(vmcall, "event", id=str(2 + i),
                         logical="smp_signal_sm_" + str(i).zfill(2),
                         physical="linux_smp_signal_sm_" + str(i).zfill(2))


def create_dbg_resources(subject):
    comp = subject.xpath("component")[0]
    for i in range(1, cores + 1):
        etree.SubElement(comp, "map", logical="log_channel" + str(2 + i),
                         physical="debuglog_subject" + str(dbgchannel_end + i))


def create_subjects(subjects, parser):
    lnx_orig = etree.parse("subj_lnx.xml", parser).getroot()
    sm_orig = etree.parse("subj_sm.xml", parser).getroot()
    for i in range(1, cores + 1):
        lnx = deepcopy(lnx_orig)
        lnx.set("name", "linux_core" + str(i))
        sm = deepcopy(sm_orig)
        sm.set("name", "sm_core" + str(i))

        subs = [lnx, sm]
        for new_subj in subs:
            nodes = new_subj.xpath("//*[@attrset]")
            for n in nodes:
                attr = n.get("attrset")
                val_exp = n.get("attrvalue")
                if not val_exp:
                    n.set(attr, n.get(attr) + str(i))
                else:
                    ldict = locals()
                    exec(val_exp, {'i': i, 'dbgchannel_end': dbgchannel_end},
                         ldict)
                    f = ldict['f']
                    n.set(attr, n.get(attr) + f)
                    del n.attrib["attrvalue"]
                del n.attrib["attrset"]

            subjects.append(new_subj)


def adjust_core_one(cpus):
    for c in cpus:
        c.xpath("minorFrame[@subject='dbgserver']")[0].set("ticks", "1")
        c.xpath("minorFrame[@subject='time']")[0].set("ticks", "1")
        for i in range(4):
            c.insert(0, etree.Element("minorFrame", subject="linux_core1",
                     ticks="10"))
        for i in range(5):
            c.append(etree.Element("minorFrame", subject="linux_core1",
                     ticks="10"))
        c.append(etree.Element("minorFrame", subject="linux_core1",
                 ticks="7"))


def create_additional_cores(majors):
    for i in range(2, cores + 1):
        for m in majors:
            cpu = etree.Element("cpu", id=str(i))
            for j in range(10):
                etree.SubElement(cpu, "minorFrame",
                                 subject="linux_core" + str(i), ticks="10")
            m.append(cpu)


def spread_devices():
    lnx = doc.xpath("/system/subjects/subject[@name='linux']")[0]
    devs = lnx.xpath("devices/device")
    for d in devs:
        d.getparent().remove(d)
    subjs = doc.xpath("/system/subjects/subject[starts-with(@name, 'linux')]")

    subj_max = len(subjs)
    cur_subj = 0

    for d in devs:
        subjs[cur_subj].xpath("devices")[0].append(d)
        cur_subj += 1
        if cur_subj == subj_max:
            cur_subj = 0


parser = etree.XMLParser(remove_blank_text=True)
doc = etree.parse("../demo_system_desktop.xml", parser).getroot()

create_phys_events(doc.xpath("/system/events")[0])
create_phys_channels(doc.xpath("/system/channels")[0])
create_lnx_resources(doc.xpath("/system/subjects/subject[@name='linux']")[0])
create_dbg_resources(
    doc.xpath("/system/subjects/subject[@name='dbgserver']")[0])
create_subjects(doc.xpath("/system/subjects")[0], parser)
adjust_core_one(doc.xpath("/system/scheduling/majorFrame/cpu[@id='1']"))
create_additional_cores(doc.xpath("/system/scheduling/majorFrame"))
spread_devices()

with open('../demo_system_desktop_smp.xml', 'wb') as f:
    f.write(etree.tostring(doc, pretty_print=True))
