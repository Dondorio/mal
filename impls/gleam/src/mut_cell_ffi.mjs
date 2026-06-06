export function new_cell(value) {
  return { value };
}

export function get(cell) {
  return cell.value;
}

export function set(cell, value) {
  cell.value = value;
  return undefined;
}

export function update(cell, func) {
  cell.value = func(cell.value);
  return undefined;
}
