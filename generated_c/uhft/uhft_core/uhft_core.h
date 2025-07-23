/*
 * Generated OWL C Header
 * Timestamp: 2025-07-22T23:59:38.223217
 * Compiler: OWL AOT Compiler with Jinja 1.0.0
 */

#ifndef UHFT_CORE_H
#define UHFT_CORE_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Ontology Metadata */
#define ONTOLOGY_VERSION "1.0.0"
#define ONTOLOGY_TIMESTAMP "2025-07-22T23:59:38.217024"
#define TOTAL_CLASSES 4
#define TOTAL_PROPERTIES 4
#define TOTAL_RULES 0

/* Namespace Prefixes */
#define PREFIX_BRICK_URI "https://brickschema.org/schema/Brick#"
#define PREFIX_CSVW_URI "http://www.w3.org/ns/csvw#"
#define PREFIX_DC_URI "http://purl.org/dc/elements/1.1/"
#define PREFIX_DCAT_URI "http://www.w3.org/ns/dcat#"
#define PREFIX_DCMITYPE_URI "http://purl.org/dc/dcmitype/"
#define PREFIX_DCTERMS_URI "http://purl.org/dc/terms/"
#define PREFIX_DCAM_URI "http://purl.org/dc/dcam/"
#define PREFIX_DOAP_URI "http://usefulinc.com/ns/doap#"
#define PREFIX_FOAF_URI "http://xmlns.com/foaf/0.1/"
#define PREFIX_GEO_URI "http://www.opengis.net/ont/geosparql#"
#define PREFIX_ODRL_URI "http://www.w3.org/ns/odrl/2/"
#define PREFIX_ORG_URI "http://www.w3.org/ns/org#"
#define PREFIX_PROF_URI "http://www.w3.org/ns/dx/prof/"
#define PREFIX_PROV_URI "http://www.w3.org/ns/prov#"
#define PREFIX_QB_URI "http://purl.org/linked-data/cube#"
#define PREFIX_SCHEMA_URI "https://schema.org/"
#define PREFIX_SKOS_URI "http://www.w3.org/2004/02/skos/core#"
#define PREFIX_SOSA_URI "http://www.w3.org/ns/sosa/"
#define PREFIX_SSN_URI "http://www.w3.org/ns/ssn/"
#define PREFIX_TIME_URI "http://www.w3.org/2006/time#"
#define PREFIX_VANN_URI "http://purl.org/vocab/vann/"
#define PREFIX_VOID_URI "http://rdfs.org/ns/void#"
#define PREFIX_WGS_URI "https://www.w3.org/2003/01/geo/wgs84_pos#"
#define PREFIX_OWL_URI "http://www.w3.org/2002/07/owl#"
#define PREFIX_RDF_URI "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
#define PREFIX_RDFS_URI "http://www.w3.org/2000/01/rdf-schema#"
#define PREFIX_XSD_URI "http://www.w3.org/2001/XMLSchema#"
#define PREFIX_XML_URI "http://www.w3.org/XML/1998/namespace"
#define PREFIX_CNS_URI "http://cns.io/ontology#"
#define PREFIX_EH_URI "http://cns.io/eightfold#"
#define PREFIX_SHACL_URI "http://www.w3.org/ns/shacl#"
#define PREFIX__URI "http://cns.io/uhft#"

/* Forward Declarations */
typedef struct Trading_Order_s Trading_Order_t;
typedef struct Order_Book_s Order_Book_t;
typedef struct Matching_Engine_s Matching_Engine_t;
typedef struct Market_Data_s Market_Data_t;

/* Eightfold Path Stages */
typedef enum {
    EIGHTFOLD_RIGHT_UNDERSTANDING,
    EIGHTFOLD_RIGHT_THOUGHT,
    EIGHTFOLD_RIGHT_SPEECH,
    EIGHTFOLD_RIGHT_ACTION,
    EIGHTFOLD_RIGHT_LIVELIHOOD,
    EIGHTFOLD_RIGHT_EFFORT,
    EIGHTFOLD_RIGHT_MINDFULNESS,
    EIGHTFOLD_RIGHT_CONCENTRATION,
    EIGHTFOLD_STAGE_COUNT
} eightfold_stage_t;

/* Property Types */
typedef enum {
    PROPERTY_TYPE_OBJECT,
    PROPERTY_TYPE_DATATYPE,
    PROPERTY_TYPE_ANNOTATION
} property_type_t;

/* Property Characteristics */
typedef enum {
    PROP_FUNCTIONAL = 1 << 0,
    PROP_INVERSE_FUNCTIONAL = 1 << 1,
    PROP_TRANSITIVE = 1 << 2,
    PROP_SYMMETRIC = 1 << 3,
    PROP_ASYMMETRIC = 1 << 4,
    PROP_REFLEXIVE = 1 << 5,
    PROP_IRREFLEXIVE = 1 << 6
} property_characteristics_t;

/* Base OWL Object */
typedef struct {
    const char* uri;
    const char* label;
    const char* comment;
    eightfold_stage_t eightfold_stage;
    uint32_t type_id;
    void* instance_data;
} owl_object_t;

/* Property Descriptor */
typedef struct {
    const char* uri;
    const char* label;
    property_type_t type;
    property_characteristics_t characteristics;
    const char** domain_classes;
    const char** range_classes;
    const char* inverse_property;
    size_t domain_count;
    size_t range_count;
} property_descriptor_t;

/* Class Definitions */
/*  * Represents a trading order with 8-tick execution guarantee */
struct Trading_Order_s {
    owl_object_t base;
    
    /* Parent class data */
    BitActor_t* bit_actor_parent;
    
    /* Properties */
    double order_price;
    int32_t order_quantity;
    long_t* order_timestamp;
    
    /* Constraint validation flags */
    
    /* Optimization hints */
    struct {
        bool memory_layout_enabled;
    } optimization;
};

/* Constructor/Destructor for Trading Order */
Trading_Order_t* trading_order_create(void);
void trading_order_destroy(Trading_Order_t* obj);
bool trading_order_validate(const Trading_Order_t* obj);

/*  * Lock-free order book implementation */
struct Order_Book_s {
    owl_object_t base;
    
    /* Parent class data */
    Arena_t* arena_parent;
    
    /* Properties */
    
    /* Constraint validation flags */
    
    /* Optimization hints */
    struct {
        bool memory_layout_enabled;
    } optimization;
};

/* Constructor/Destructor for Order Book */
Order_Book_t* order_book_create(void);
void order_book_destroy(Order_Book_t* obj);
bool order_book_validate(const Order_Book_t* obj);

/*  * Ultra-low-latency order matching engine */
struct Matching_Engine_s {
    owl_object_t base;
    
    /* Parent class data */
    RingBus_t* ring_bus_parent;
    
    /* Properties */
    int32_t execution_latency;
    
    /* Constraint validation flags */
    
    /* Optimization hints */
    struct {
        bool memory_layout_enabled;
    } optimization;
};

/* Constructor/Destructor for Matching Engine */
Matching_Engine_t* matching_engine_create(void);
void matching_engine_destroy(Matching_Engine_t* obj);
bool matching_engine_validate(const Matching_Engine_t* obj);

/*  * Real-time market data feed */
struct Market_Data_s {
    owl_object_t base;
    
    /* Parent class data */
    Fiber_t* fiber_parent;
    
    /* Properties */
    
    /* Constraint validation flags */
    
    /* Optimization hints */
    struct {
        bool memory_layout_enabled;
    } optimization;
};

/* Constructor/Destructor for Market Data */
Market_Data_t* market_data_create(void);
void market_data_destroy(Market_Data_t* obj);
bool market_data_validate(const Market_Data_t* obj);


/* Property Descriptors Array */
extern const property_descriptor_t g_property_descriptors[4];

/* Class Registry */
typedef struct {
    const char* uri;
    const char* label;
    size_t instance_size;
    eightfold_stage_t eightfold_stage;
    const char** parent_classes;
    size_t parent_count;
    owl_object_t* (*constructor)(void);
    void (*destructor)(owl_object_t*);
    bool (*validator)(const owl_object_t*);
} class_descriptor_t;

extern const class_descriptor_t g_class_descriptors[4];

/* Reasoning Engine */
typedef struct {
    const char* id;
    const char* type;
    float confidence;
    eightfold_stage_t stage;
    bool (*apply_rule)(owl_object_t* subject);
} reasoning_rule_t;

extern const reasoning_rule_t g_reasoning_rules[0];

/* API Functions */
owl_object_t* owl_create_instance(const char* class_uri);
void owl_destroy_instance(owl_object_t* obj);
bool owl_validate_instance(const owl_object_t* obj);
const property_descriptor_t* owl_get_property(const char* property_uri);
const class_descriptor_t* owl_get_class(const char* class_uri);
bool owl_apply_reasoning(owl_object_t* obj);
eightfold_stage_t owl_get_eightfold_stage(const char* class_uri);

/* Eightfold Path Integration */
typedef struct {
    eightfold_stage_t stage;
    owl_object_t** instances;
    size_t instance_count;
    size_t capacity;
} eightfold_context_t;

eightfold_context_t* eightfold_create_context(void);
void eightfold_destroy_context(eightfold_context_t* ctx);
bool eightfold_add_instance(eightfold_context_t* ctx, owl_object_t* obj);
owl_object_t** eightfold_get_stage_instances(eightfold_context_t* ctx, eightfold_stage_t stage);
bool eightfold_execute_stage(eightfold_context_t* ctx, eightfold_stage_t stage);

#ifdef __cplusplus
}
#endif

#endif /* UHFT_CORE_H */
