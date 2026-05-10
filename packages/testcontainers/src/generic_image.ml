open Std

module ReadinessPolicy = Readiness_policy

module Duration = struct
  include Time.Duration

  let of_secs = from_secs

  let of_millis = from_millis
end

type t = {
  name: string;
  tag: string;
  cmd: string list;
  env: (string * string) list;
  labels: (string * string) list;
  exposed_ports: Docker_client.Port.t list;
  port_mappings: Docker_client.Container.port_mapping list;
  readiness_policies: ReadinessPolicy.t list;
}

let make = fun name tag ->
  {
    name;
    tag;
    cmd = [];
    env = [];
    labels = [];
    exposed_ports = [];
    port_mappings = [];
    readiness_policies = [];
  }

let tcp = fun port -> Docker_client.Port.tcp port

let with_cmd = fun ~cmd image -> { image with cmd }

let with_env_var = fun ~name ~value image ->
  {
    image with
    env = (name, value) :: image.env;
  }

let with_label = fun ~name ~value image ->
  {
    image with
    labels = (name, value) :: image.labels;
  }

let with_exposed_port = fun ~port image -> {
  image with
  exposed_ports = tcp port :: image.exposed_ports;
}

let with_exposed_docker_port = fun ~port image -> {
  image with
  exposed_ports = port :: image.exposed_ports;
}

let with_mapped_port = fun ~host_port ~container_port image -> {
  image with
  port_mappings = { Docker_client.Container.host_port; container_port = tcp container_port }
  :: image.port_mappings;
}

let with_mapped_docker_port = fun ~host_port ~container_port image -> {
  image with
  port_mappings = { Docker_client.Container.host_port; container_port } :: image.port_mappings;
}

let with_readiness_policy = fun ~policy image -> {
  image with
  readiness_policies = image.readiness_policies @ [ policy ];
}

let descriptor = fun image -> image.name ^ ":" ^ image.tag
