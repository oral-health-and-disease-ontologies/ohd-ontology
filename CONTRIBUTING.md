# Contributing to the OHD Ontology
## Configuring Protégé for OHD contributions

### Rationale
When a new entity is created using Protégé the entities are created using the default settings of Protégé. For example, using the default settings when a new entity called 'test' is created the IRI for the entity may look like this.

`http://www.semanticweb.org/user/ontologies/2023/6/untitled-ontology-43#test`

Ideally, we want the new entity created in OHD to look similar to the one below based on OBO Foundry principles:

`http://purl.obolibrary.org/obo/OHD_0000024`

### Prerequisites
#### Protégé
If you do not have installed Protégé ontology editor on your machine, please download and install. Protégé can be downloaded here:

https://protege.stanford.edu/

#### Create ORCID identifier
If you do not have an ORCID ID please create one by registering here:

https://orcid.org/

ORCID ID is a free-of-charge identifier for researchers. For more information about ORCID see below.

https://info.orcid.org/what-is-orcid/

### Update Preferences

#### Where can I find Protégé preferences?
- Open Protégé.
- Go to File > Preferences.

**IMAGE**

#### Update user details in Protégé
- Go to Protégé preferences.
- Click on the 'User details' tab.
- Enable 'Use supplied user name'.
- Enter your full name in the field indicated.
- Enable 'Use Git user name when available'.
- Enter your ORCID ID in the field for ORCID.
- Click OK to save.

![user_details_edit](https://github.com/oral-health-and-disease-ontologies/ohd-ontology/assets/47677575/8e452096-f685-4066-9d04-f0417451c456)

#### Update configuration for new entities
- Go to Protégé preferences.
- Click on the 'New entities' tab.
- Update 'Entity IRI'
  - For 'Start with:' enable 'Specified IRI' and enter the following IRI in the field:
    - `http://purl.obolibrary.org/obo`
  - For 'Followed by:' enable the forward slash option (/).
  - For 'End with:' enable 'Auto-generated ID'.
- Update 'Entity Label'
  - Enable 'Same as label renderer'. 
- Update 'Auto-generated ID'
  - Enable 'Numeric (Iterative)'
  - For 'Prefix:' enter 'OHD_'in the field provided.
  - For 'Start:' enter the lower value integer of the ID range if one has been assigned to you. For example 2,000. Most regular contributors are assigned an ID range. 
  - For 'End:' enter the upper-value integer of the ID range if one has been assigned to you. For example 2,999.
  - Enable 'Remember last ID between Protégé sessions'.
- Click OK to save.

![new_entities](https://github.com/oral-health-and-disease-ontologies/ohd-ontology/assets/47677575/745d1eb1-6169-4401-bfc4-9bed8a9e2bf9)

#### Update configuration for new entities metadata
- Go to Protégé preferences.
- Click on the 'New entities metadata' tab.
- Enable 'Annotate new entities with creator (user)'.
- For 'Creator property' enter the following IRI the field:
  - `http://purl.org/dc/terms/contributor`
- For 'Creator value' enable 'Use ORCID'.
- Enable 'Annotate new entities with creation date/time'. 
- For 'Date property' enter the following IRI the field:
  - `http://purl.org/dc/terms/created`
- For 'Date property' enable 'ISO-8601'.
- Click OK to save.


![new_entities_metadata](https://github.com/oral-health-and-disease-ontologies/ohd-ontology/assets/47677575/74d66507-de94-471e-81cc-959caddcff82)


