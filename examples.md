# metadataeditr examples

Set your API endpoint and key first:

```r
me_set_api("https://your-editor.example.org/index.php/api/", "YOUR_API_KEY")
```

## Template API examples

List templates:

```r
templates <- me_template_list()
```

Create a template:

```r
new_template <- me_template_create(
  template_data = list(
    title = "My template",
    study_type = "survey",
    metadata_template = list()
  )
)
```

Get a template by UID:

```r
template_item <- me_template_get(uid = "template-uid")
```

Duplicate a template:

```r
dup_template <- me_template_duplicate(uid = "template-uid")
```

Update a template:

```r
updated_template <- me_template_update(
  uid = "template-uid",
  template_data = list(
    title = "Updated template title"
  )
)
```

Delete a template:

```r
deleted_template <- me_template_delete(uid = "template-uid")
```

Get translation keys:

```r
translation_keys <- me_template_translation_keys(
  uid = "template-uid",
  format = "compact"
)
```

## Collections API examples

List collections:

```r
collections <- me_collection_list()
```

Create a collection:

```r
created_collection <- me_collection_create(
  collection_data = list(
    title = "My collection",
    description = "Collection created from R client"
  )
)
```

Get/update/delete a collection:

```r
collection_item <- me_collection_get(collection_id = 123)

updated_collection <- me_collection_update(
  collection_id = 123,
  collection_data = list(
    title = "Updated collection title"
  )
)

deleted_collection <- me_collection_delete(collection_id = 123)
```

Collection copy/move:

```r
copied_collection <- me_collection_copy(
  copy_data = list(
    source_collection_id = 123,
    target_parent_id = 10
  )
)

moved_collection <- me_collection_move(
  move_data = list(
    collection_id = 123,
    target_parent_id = 10
  )
)
```

Projects and project access:

```r
linked_projects <- me_collection_projects(
  projects_data = list(
    collections = c(123),
    projects = c("PROJECT-IDNO-1", "PROJECT-IDNO-2"),
    id_format = "idno"
  )
)

project_access <- me_collection_user_project_access_get(collection_id = 123)

added_access <- me_collection_user_project_access_add(
  access_data = list(
    collection_id = 123,
    user_id = 55,
    project_id = "PROJECT-IDNO-1"
  )
)

removed_access <- me_collection_user_project_access_remove(
  access_data = list(
    collection_id = 123,
    user_id = 55,
    project_id = "PROJECT-IDNO-1"
  )
)
```

ACL and permissions:

```r
permissions <- me_collection_permissions()

acl_entries <- me_collection_user_acl_get(collection_id = 123)

acl_add <- me_collection_user_acl_add(
  acl_data = list(collection_id = 123, user_id = 55, permissions = c("view", "edit"))
)

acl_update <- me_collection_user_acl_update(
  acl_data = list(collection_id = 123, user_id = 55, permissions = c("view"))
)

acl_remove <- me_collection_user_acl_remove(
  acl_data = list(collection_id = 123, user_id = 55)
)

acl_check <- me_collection_user_acl_check(collection_id = 123, user_id = 55)
```

Collection templates and project mapping:

```r
collection_template <- me_collection_template(
  template_data = list(
    title = "Collection template",
    template_uid = "template-uid"
  )
)

project_collections <- me_collection_by_project(project_id = "PROJECT-IDNO-1")
```

## External resources API examples

List resources for a project:

```r
resources <- me_resource_list(idno = "PROJECT-IDNO-1")
```

Add / update / delete / import RDF:

```r
added <- me_resource_add(
  idno = "PROJECT-IDNO-1",
  dctype = "Documentation",
  title = "My resource"
)

# Replace existing resource that already uses the same stored filename (optional)
added_replace <- me_resource_add(
  idno = "PROJECT-IDNO-1",
  dctype = "Documentation",
  title = "My resource",
  file_path = "/path/to/file.pdf",
  overwrite = TRUE
)

updated <- me_resource_update(
  idno = "PROJECT-IDNO-1",
  resource_id = 101,
  title = "Renamed resource"
)

deleted <- me_resource_delete(idno = "PROJECT-IDNO-1", resource_id = 101)

imported <- me_resource_import_rdf(
  idno = "PROJECT-IDNO-1",
  rdf_file = "/path/to/resources.rdf"
)
```

Generate or download RDF/JSON bundles (paths match OpenAPI: `resources/write_json{projectId}`, etc.):

```r
gen_json <- me_resource_write_json(idno = "123")

gen_rdf <- me_resource_write_rdf(idno = "123")

rdf_dl <- me_resource_rdf(idno = "123", as_text = TRUE)

if (!is.null(rdf_dl$content_type)) {
  message(paste("Content-Type:", rdf_dl$content_type))
}
```

## Indicator DSD workflow (global registry)

Import a catalogue DSD, bind it to an indicator project, and publish CSV data:

```r
me_set_api("https://your-editor.example.org/index.php/api/", "YOUR_API_KEY")

ds_ref <- me_data_structure_reference(
  idno = "MY_DSD_V1", agency = "NADA", name = "MY_STRUCTURE", version = "1.0"
)

me_data_structure_import_json(payload = list(
  data_structure = list(
    idno = "MY_DSD_V1", name = "MY_STRUCTURE", agency = "NADA", version = "1.0",
    components = list(
      list(name = "REF_AREA", column_type = "geography", data_type = "string"),
      list(name = "TIME_PERIOD", column_type = "time_period", data_type = "integer",
           time_period_format = "YYYY"),
      list(name = "INDICATOR", column_type = "indicator_id", data_type = "string"),
      list(name = "OBS_VALUE", column_type = "observation_value", data_type = "double")
    )
  ),
  dry_run = TRUE
))

me_create_project("indicator", "MY-IND-001", metadata = list(
  series_description = list(idno = "MY-IND-001", name = "Example indicator")
))

me_indicator_dsd_bind("MY-IND-001", data_structure_reference = ds_ref, indicator_id_value = "CODE1")
me_indicator_dsd_update_binding("MY-IND-001", implied_freq_code = "A")

me_indicator_publish_data("MY-IND-001", csv_file = "data.csv")
me_indicator_dsd_validate("MY-IND-001")
```

See the runnable script [`examples/indicator_dsd_full_workflow.R`](examples/indicator_dsd_full_workflow.R).
