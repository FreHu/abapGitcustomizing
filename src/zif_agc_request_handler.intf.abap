INTERFACE zif_agc_request_handler
  PUBLIC .

  METHODS select.

  METHODS get_data
    RETURNING VALUE(rt_customizing_ui) TYPE zif_agc_ui=>ty_t_customizing_ui.

ENDINTERFACE.
