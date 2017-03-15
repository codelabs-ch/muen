// segment descriptors: See Intel SDM 3A, chapter 3.4.5

#define GDTE_TYPE_DATA_W (1 <<  9)
#define GDTE_CODE        (1 << 11)
#define GDTE_NON_SYSTEM  (1 << 12)
#define GDTE_PRESENT     (1 << 15)
#define GDTE_LONG        (1 << 21)
#define GDTE_32BIT       (1 << 22)
#define GDTE_GRANULAR_4K (1 << 23)

#define GDTE_SYS_TSS 0x900

#define AP_KERN_CS 0x8
