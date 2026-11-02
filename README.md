# metadataeditr

R client for the Metadata Editor HTTP API.


# Installation

## Install package

To install the package from GitHub, follow the steps:

**Install the devtools package**

```r
install.packages("devtools")
```

**Load the devtools package**

```r
library(devtools)
```

**Install metadataeditr**

```r
install_github("ihsn/metadataeditr")
```

Load the package:

```r
library(metadataeditr)
```


# Quick start

```r
me_set_api("https://your-editor.example.org/index.php/api/", "YOUR_API_KEY")
```

Alternatively set URL and key separately: `me_set_api_url()`, `me_set_api_key()`.

For usage examples by area, see [`examples.md`](examples.md).

For the full **global DSD → indicator project → bind → validate → upload** workflow, see
[`examples/indicator_dsd_full_workflow.R`](examples/indicator_dsd_full_workflow.R).
For a complete workflow that creates a codelist and DSD, creates and binds an indicator project,
imports observations, and reads/exports the resulting data, see
[`examples/indicator_end_to_end.R`](examples/indicator_end_to_end.R).


# Exported functions

Short reference for package exports. Paths and payloads match the Editor OpenAPI unless noted.

### API configuration

| Function | Description |
|----------|-------------|
| `me_set_api` | Set API base URL, key, and verbose flag in one call |
| `me_set_api_url` | Store the API base URL for subsequent requests |
| `me_get_api_url` | Build full URL from base plus optional endpoint path segment |
| `me_set_api_key` | Store the API key for `X-API-KEY` |
| `me_get_api_key` | Read the configured API key |
| `me_set_api_verbose` | Turn httr verbose logging on/off for requests |
| `me_get_api_verbose` | Read API verbose flag |
| `me_get_verbose` | Read verbose flag used by wrappers |

### HTTP helpers

| Function | Description |
|----------|-------------|
| `me_metadataedit_http_get` | Generic GET with API key headers |
| `me_metadataedit_http_post` | Generic POST; `encode` = `json` (default), `form`, `multipart`, or `raw`; JSON uses `content_type_json()` + `accept_json()` |
| `me_metadataedit_http_put` | Generic PUT with optional body |
| `me_metadataedit_http_delete` | Generic DELETE with optional body |

### Projects

| Function | Description |
|----------|-------------|
| `me_list_projects` | Paginated project search/filter list |
| `me_create_project` | Create a project by type (`editor/create/{type}`) |
| `me_update_project` | Full metadata replace for a project |
| `me_patch_project` | JSON Patch–style partial metadata update |
| `me_project_by_id` | Fetch project by numeric ID |
| `me_project_by_idno` | Fetch project by idno |
| `me_import_project` | Import project from uploaded file or package |
| `me_capture_pdf_cover` | Render first page of a PDF to a JPG cover |
| `me_ProjectTypes` | Named mapping of UI labels to API project type strings |

### Global data structures (DSD registry)

| Function | Description |
|----------|-------------|
| `me_data_structure_reference` | Build `data_structure_reference` identity object |
| `me_data_structure_import_json` | `POST /data_structures/import_json` (supports `dry_run`) |
| `me_data_structure_validate` | `GET /data_structures/validate/{id}` |
| `me_data_structure_list` | `GET /data_structures` |
| `me_data_structure_get` | `GET /data_structures/single/{id}` |
| `me_data_structure_export` | `GET /data_structures/export/{segment}` |
| `me_data_structure_by_identity` | Resolve a DSD by agency, name, and version |
| `me_data_structure_lookup` | Resolve a DSD by numeric ID or idno |
| `me_data_structure_create` / `me_data_structure_update` / `me_data_structure_delete` | Create, update, and delete a registry DSD |
| `me_data_structure_components` | Search registry components |
| `me_data_structure_component_add` / `me_data_structure_component_update` / `me_data_structure_component_delete` | Manage DSD components |
| `me_data_structure_duplicate` / `me_data_structure_versions` / `me_data_structure_projects` | Duplicate a DSD, list versions, and list linked projects |

### Codelists

| Function | Description |
|----------|-------------|
| `me_codelist_list` / `me_codelist_versions` | List codelists and versions |
| `me_codelist_get` / `me_codelist_by_identity` | Get a codelist by ID or SDMX identity |
| `me_codelist_create` / `me_codelist_update` / `me_codelist_delete` | Manage codelist metadata |
| `me_codelist_codes` / `me_codelist_code_add` / `me_codelist_code_update` / `me_codelist_code_delete` | List and manage codes |
| `me_codelist_code_label_set` / `me_codelist_code_label_delete` | Manage code labels/translations |
| `me_codelist_translations` / `me_codelist_translation_set` | List and set codelist language labels |
| `me_codelist_import_json` / `me_codelist_export_json` | Import and export a codelist as JSON |

### Indicator DSD (project binding, validation, data)

| Function | Description |
|----------|-------------|
| `me_indicator_dsd_bind` | `POST /indicator_dsd/bind_global/{sid}` |
| `me_indicator_dsd_update_binding` | Set `indicator_id_value` / `implied_freq_code` |
| `me_indicator_dsd_binding` | Import readiness and structure validation summary |
| `me_indicator_dsd_validate` | Structure + published data validation report |
| `me_indicator_dsd_upload_prepare` | Multipart CSV stage + header validation |
| `me_indicator_dsd_upload_import` | Import staged CSV into DuckDB timeseries |
| `me_indicator_publish_data` | Prepare + import in one call |
| `me_indicator_dsd_list` / `me_indicator_dsd_column_get` | Read project DSD columns |
| `me_indicator_dsd_unbind` | Remove the DSD binding and published data |
| `me_indicator_data_values` / `me_indicator_data_rows` | Read distinct values or filtered, paginated observations |
| `me_indicator_data_export` / `me_indicator_data_delete` | Export observations to CSV or delete published observations |

### Indicator data & background jobs

| Function | Description |
|----------|-------------|
| `me_upload_file_chunked` | Chunked upload of a CSV tied to a project `sid` |
| `me_indicator_import_job_submit` | Queue `jobs/import_indicator_data` after upload |
| `me_import_indicator_data` | Upload CSV, submit job, poll until terminal state |
| `me_job_publish_to_nada` | Queue `jobs/publish_to_nada` (publish project to NADA catalog) |
| `me_job_wait` | Poll `/jobs/{uuid}` until completed, failed, or timeout |

### Collections

| Function | Description |
|----------|-------------|
| `me_collection_list` | List collections (GET `/collections`) |
| `me_collection_create` | Create a collection |
| `me_collection_get` | Get one collection by ID |
| `me_collection_update` | Update collection metadata |
| `me_collection_delete` | Delete a collection |
| `me_collection_copy` | Copy collection to another parent |
| `me_collection_move` | Move collection in the hierarchy |
| `me_collection_permissions` | User’s collection permission summary |
| `me_collection_projects` | Link projects to collections |
| `me_collection_add_projects` | Add projects to collections (`add_projects`) |
| `me_collection_remove_projects` | Remove projects from collections |
| `me_collection_template` | Create/use collection template payloads |
| `me_collection_user_project_access_get` | List user–project access for a collection |
| `me_collection_user_project_access_add` | Grant user project access |
| `me_collection_user_project_access_remove` | Revoke user project access |
| `me_collection_user_acl_get` | List ACL rows for a collection |
| `me_collection_user_acl_add` | Add ACL entry |
| `me_collection_user_acl_update` | Update ACL entry |
| `me_collection_user_acl_remove` | Remove ACL entry |
| `me_collection_user_acl_check` | Resolve effective ACL for a user |
| `me_collection_by_project` | Collections attached to a project (`editor/collections/{id}`) |

### Templates

| Function | Description |
|----------|-------------|
| `me_template_list` | List metadata form templates |
| `me_template_create` | Create a template |
| `me_template_get` | Get template by UID |
| `me_template_duplicate` | Duplicate template by UID |
| `me_template_update` | Update template by UID |
| `me_template_delete` | Delete template by UID |
| `me_template_translation_keys` | Translation keys for a template (`format`: full / compact) |

### External resources

| Function | Description |
|----------|-------------|
| `me_resource_list` | List external resources for a project |
| `me_resource_add` | Create resource (optional file path or URL); optional `overwrite` (`TRUE`/`FALSE`) replaces an existing resource with the same stored filename when `TRUE` |
| `me_resource_update` | Update existing resource row |
| `me_resource_delete` | Delete resource |
| `me_resource_import_rdf` | Import resources from RDF/XML via multipart upload |
| `me_resource_write_json` | Trigger JSON export/generation for resources |
| `me_resource_write_rdf` | Trigger RDF generation for resources |
| `me_resource_rdf` | Download RDF body (text or raw) |

### Uploads (resumable / chunked API)

| Function | Description |
|----------|-------------|
| `me_upload_limits` | Server limits for chunked uploads |
| `me_upload_init` | Start session (POST `/upload/{sid}`) |
| `me_upload_chunk` | Send one binary chunk with upload headers |
| `me_upload_status` | Poll session progress |
| `me_upload_delete` | Cancel/remove session |

### Thumbnails

| Function | Description |
|----------|-------------|
| `me_thumbnail_upload` | Attach thumbnail image to a project |
| `me_thumbnail_delete` | Remove custom thumbnail |

### Utilities

| Function | Description |
|----------|-------------|
| `me_is_valid_url` | Lightweight URL validation helper |
