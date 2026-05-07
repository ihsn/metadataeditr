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

### Indicator data & background jobs

| Function | Description |
|----------|-------------|
| `me_upload_file_chunked` | Chunked upload of a CSV (and related flows) tied to a project `sid` |
| `me_indicator_import_job_submit` | Queue `import_indicator_data` job after upload |
| `me_import_indicator_data` | Upload CSV, submit job, poll until terminal state |
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
| `me_resources_list` | List external resources for a project |
| `me_resources_add` | Create resource (optional file path or URL) |
| `me_resources_update` | Update existing resource row |
| `me_resources_delete` | Delete resource |
| `me_resources_import_rdf` | Import resources from RDF/XML via multipart upload |
| `me_resources_write_json` | Trigger JSON export/generation for resources |
| `me_resources_write_rdf` | Trigger RDF generation for resources |
| `me_resources_rdf` | Download RDF body (text or raw) |

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
