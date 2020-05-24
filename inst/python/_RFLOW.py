class _RFLOW_DATA:
  def __getitem__(self, key):
    rcode = 'RF$' + key + '$value'
    return R.__getitem__(code=rcode)

class _RFLOW_NODES:
  def __getitem__(self, key):
    rcode = 'RF$' + key
    return R.__getitem__(code=rcode)


_RFDATA = _RFLOW_DATA()
_RFLOW = _RFLOW_NODES()
