# AHCI Driver

## Introduction

This ** AHCI Driver ** subject 'implements support for the AHCI Block.

At the moment there are some restrictions / open items:
- Only SATA devices supported. There is no support for ATAPI.
- Only MBR partitions supported.
- Error handling is nearly untested due to missing broken hardware.

## Configure the server

The driver offers a server to handle requests from multiple client
subjects using channels. The mapping of device-partitions to different
channels (and thus to different client subjects) is made in the
ports_config.ads file.

## Usage with muenblock-client from Linux

Load the muenblock client module with the protocol Id 5155684453516f3d
for both channels. See Linux muenblock readme for details.

## Usage from a native subject

There is a muenblock-client interface. See example subject
muenblock-example how to use it.
