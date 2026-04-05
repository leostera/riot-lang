export interface PackagePipelineProcessRequest {
  package_name: string;
  package_version: string;
  artifact_sha256: string;
  source_archive_key: string;
  source_archive_url: string;
  riot_install_url?: string;
  riot_release_metadata_url?: string;
  generate_docs: boolean;
  verify_build: boolean;
}

export interface PackagePipelineCommandResult {
  success: boolean;
  exit_code: number;
  stdout: string;
  stderr: string;
  duration_ms: number;
  command: string[];
}

export interface GeneratedDocsFile {
  path: string;
  content_base64: string;
  content_type?: string;
}

export interface DocsPipelineProcessResult {
  docs?: PackagePipelineCommandResult & {
    output_dir: string;
    files: GeneratedDocsFile[];
  };
  build?: PackagePipelineCommandResult;
}

export interface PackagePipelineExecutor {
  processRelease(request: PackagePipelineProcessRequest): Promise<DocsPipelineProcessResult>;
}
